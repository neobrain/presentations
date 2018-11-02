// g++ generators.cpp -std=c++17
#include <iostream>
#include <iomanip>
#include <tuple>

#include "dummy_env.hpp"
#include "ipc.hpp"

#include "magic.hpp" // provides CallWithSequentialEvaluation

namespace FS {

struct OpenFile :
    IPCCmd<0x802>
    ::normal<uint32_t, uint32_t, uint32_t>
    ::special<WriteableBuffer>
    ::response<FileDescriptor>::fin {};

struct ReadFile :
    IPCCmd<0x803>
    ::normal<FileDescriptor, uint64_t, uint64_t>
    ::special<WriteableBuffer>
    ::response<uint32_t>::fin {};

}

IPCCmdDatabase<FS::OpenFile, FS::ReadFile> ipc_cmd_database{{"FS::OpenFile"}, {"FS::ReadFile"}};


template<typename TypeList>
struct DecodeAllAndApply;

template<typename... Ts>
struct DecodeAllAndApply<std::tuple<Ts...>> {
    uint32_t offset = 0x1; // offset into command block

    // Read a single entry from the CmdBlock and advance "offset"
    template<typename T>
    auto ReadEntry(const CmdBlock& block) {
        if constexpr (std::is_same_v<T, uint32_t>) {
            return block.ReadU32(offset++);
        } else if constexpr (std::is_same_v<T, uint64_t>) {
            uint32_t val_low  = block.ReadU32(offset++);
            uint32_t val_high = block.ReadU32(offset++);
            return (uint64_t{val_high} << 32) | val_low;
        } else if constexpr (std::is_same_v<T, WriteableBuffer>) {
            // Further validation of descriptor entry here:
            uint32_t descriptor = block.ReadU32(offset++);
            if (!IsValidBufferDescriptor(descriptor))
                throw std::runtime_error("Expected buffer descriptor");

            auto [size, flags] = DecodeBufferDescriptor(descriptor);
            uint32_t address = block.ReadU32(offset++);
            return WriteableBuffer { address, size };
        } else if constexpr (std::is_same_v<T, FileDescriptor>) {
            return FileDescriptor { block.ReadU32(offset++) };
        } else {
            struct NOTFOUND {} a {};
            return a;
        }
    }

    // Run over the entire CmdBlock, gather results and apply them to "f"
    template<typename F>
    decltype(auto) operator()(const CmdBlock& cmd_block, F&& f) {
        return CallWithSequentialEvaluation<F, Ts...> { std::forward<F>(f), ReadEntry<Ts>(cmd_block)... }.get();
    }
};

template<typename TypeList>
struct EncodeAll;

template<typename... Ts>
struct EncodeAll<std::tuple<Ts...>> {
    CmdBlock& cmd_block;
    int offset = 0x1;

    template<typename T>
    void EncodeEntry(T t) {
        if constexpr (std::is_same_v<T, uint32_t>) {
            cmd_block.WriteU32(offset++, t);
        } else if constexpr (std::is_same_v<T, uint64_t>) {
            cmd_block.WriteU32(offset++, t & 0xffffffff);
            cmd_block.WriteU32(offset++, t >> 32);
        } else if constexpr (std::is_same_v<T, WriteableBuffer>) {
            cmd_block.WriteU32(offset++, (t.size << 4) | 0x8); // descriptor
            cmd_block.WriteU32(offset++, t.address);
        } else if constexpr (std::is_same_v<T, FileDescriptor>) {
            cmd_block.WriteU32(offset++, t.fd);
        }
    }

    void operator()(Ts... ts) {
        (EncodeEntry<Ts>(ts), ...);
    }
};


/*********************************
 * GENERATOR 1: COMMAND HANDLING *
 *********************************/
template<typename Command, typename Handler>
auto GlueCommandHandler(CmdBlock& cmd_block, Handler&& handler) {
    auto request_header = cmd_block.ReadU32(0);
    if (request_header != Command::request_header)
        throw std::runtime_error("Invalid request header");

    auto results = DecodeAllAndApply<typename Command::request_list>{}(cmd_block, handler);
    cmd_block.WriteU32(std::get<0>(results), Command::response_header); // Write result code
    std::apply(EncodeAll<typename Command::response_list>{cmd_block}, results);
    return results;
}

/****************************************
 * GENERATOR 2: COMMAND BLOCK SYNTHESIS *
 ****************************************/
template<typename Command, typename... Ts>
CmdBlock CraftCommandBlock(Ts... ts) {
    CmdBlock block;
    block.WriteU32(0, Command::request_header);
    EncodeAll<typename Command::request_list>{block}(ts...);

    for (int i = 0; i < 8; ++i) {
        std::cout << "0x" << std::hex << std::setw(8) << std::setfill('0') << block.ReadU32(i) << '\n';
    }
    std::cout << std::endl;

    return block;
}

/*******************************
 * GENERATOR 3: COMMAND LOGGER *
 *******************************/
template<typename Cmd, typename... Data>
static void LogCommand(const IPCDataBaseEntry<Cmd>& entry, Data... data) {
    std::cout << "Detected IPC command \"" << entry.name << "\"" << std::endl;
    ((std::cout << std::hex << data << '\n'), ...);
    std::cout << std::endl;
}

template<typename... DBEntries>
static void DetectAndLogCommandHelper(const CmdBlock& block, std::tuple<DBEntries...>& database) {
    const uint32_t cmd_header = block.ReadU32(0);

    // Returns true on match
    auto log_if_matching = [&](auto& db_entry) -> bool {
        using Cmd = typename std::remove_reference_t<decltype(db_entry)>::Cmd;
        const bool match = (Cmd::request_header == cmd_header);
        if (match) {
            DecodeAllAndApply<typename Cmd::request_list>{}(block,
                [&](auto... data) {
                    LogCommand(db_entry, data...);
                    return 0;
                });
        }
        return match;
    };

    bool match_found = (log_if_matching(std::get<DBEntries>(database)) || ...);
    if (!match_found) {
        std::cout << "Unknown IPC command!" << std::endl;
    }
}

void DetectAndLogCommand(const CmdBlock& block) {
    DetectAndLogCommandHelper(block, ipc_cmd_database);
}


/********************
 * EXAMPLE HANDLERS *
 ********************/
std::tuple<uint32_t, FileDescriptor>
DoOpenFile(uint32_t io_flags, uint32_t attributes, uint32_t path_size, WriteableBuffer path) {
    std::cout << "Called DoOpenFile!" << std::endl;

    // Return some dummy file descriptor
    return { 0, FileDescriptor { 0x12345 } };
};

std::tuple<uint32_t, uint32_t>
DoReadFile(FileDescriptor fd, uint64_t offset, uint64_t size, WriteableBuffer buf) {
    std::cout << "Called DoReadFile!" << std::endl;
    return { 0, size };
};

int main() {
    WriteableBuffer buf { 0x1ff00200, 0x400 };

    CmdBlock cmd_block;

    // OpenFile examples
    std::cout << "CraftCommandBlock:\n";
    cmd_block = CraftCommandBlock<FS::OpenFile>(0x3, 0x1100, buf.size, buf);
    getc(stdin);

    std::cout << "DetectAndLogCommand:\n";
    DetectAndLogCommand(cmd_block);
    getc(stdin);

    std::cout << "GlueCommandHandler:\n";
    auto [result, file_descriptor] = GlueCommandHandler<FS::OpenFile>(cmd_block, DoOpenFile);
    getc(stdin);

    std::cout << "\n\n";

    // ReadFile examples
    std::cout << "CraftCommandBlock:\n";
    cmd_block = CraftCommandBlock<FS::ReadFile>(file_descriptor, 1, 0x200, buf);
    getc(stdin);

    std::cout << "DetectAndLogCommand:\n";
    DetectAndLogCommand(cmd_block);
    getc(stdin);

    std::cout << "GlueCommandHandler:\n";
    GlueCommandHandler<FS::ReadFile>(cmd_block, DoReadFile);
    getc(stdin);


    // For the sake of completeness...
    std::cout << "\n\nCorrupting CmdBlock, then calling DetectAndLogCommand:\n";
    cmd_block.WriteU32(0, 0xbadb01);
    DetectAndLogCommand(cmd_block);
}
