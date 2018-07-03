// g++ generators.cpp -std=c++11
#include <cstdint>
#include <iostream>
#include <tuple>
#include "magic.hpp" // provides CallWithSequentialEvaluation

/*********************
 * DUMMY ENVIRONMENT *
 *********************/
struct CmdBlock {
    uint32_t data[16];

    uint32_t ReadU32(size_t off) const { return data[off]; }
};

struct WriteableBuffer {
    uint32_t address;
    uint32_t size;
};
std::ostream& operator<<(std::ostream& os, WriteableBuffer& buf) {
    os << "WriteableBuffer { " << buf.address << ", " << buf.size << " }";
    return os;
}

bool IsValidBufferDescriptor(uint32_t descriptor) {
    return ((descriptor & 0x8) != 0);
}

std::pair<uint32_t, uint32_t> DecodeBufferDescriptor(uint32_t descriptor) {
    return { descriptor >> 4 /* size */,
             descriptor & 0xf /* flags */ };
}

template<typename TypeList>
struct DecodeAllAndApply;

template<typename... T>
struct DecodeAllAndApply<std::tuple<T...>> {
    uint32_t offset = 0x1; // offset into command block

    // Read a single entry from the CmdBlock and advance "offset"
    template<typename T2>
    auto ReadEntry(CmdBlock& block) {
        if constexpr (std::is_same_v<T2, uint32_t>) {
            return block.ReadU32(offset++);
        } else if constexpr (std::is_same_v<T2, uint64_t>) {
            uint32_t val_low  = block.ReadU32(offset++);
            uint32_t val_high = block.ReadU32(offset++);
            return (uint64_t{val_high} << 32) | val_low;
        } else if constexpr (std::is_same_v<T2, WriteableBuffer>) {
            // Further validation of descriptor entry here:
            uint32_t descriptor = block.ReadU32(offset++);
            if (!IsValidBufferDescriptor(descriptor))
                throw std::runtime_error("Expected buffer descriptor");

            auto [size, flags] = DecodeBufferDescriptor(descriptor);
            uint32_t address = block.ReadU32(offset++);
            return WriteableBuffer { address, size };
        }
    }

    // Run over the entire CmdBlock, gather results and apply them to "f"
    template<typename F>
    auto operator()(CmdBlock& cmd_block, F&& f) {
        return CallWithSequentialEvaluation { f, ReadEntry<T>(cmd_block)... };
    }
};

int DoStuff(uint32_t a, uint64_t b, WriteableBuffer c) {
    std::cout << "Hello World" << std::endl;
    std::cout << std::hex << a << std::endl;
    std::cout << b << std::endl;
    std::cout << c << std::endl;
    return 0;
}

int main() {
    CmdBlock cmd_block = {{
        0x0802'02'03, // cmd_id 0x802, 3 normal parameters
        0xdeadb00f,
        0xabad1dea,
        0x22222222,
        0xc | (0x345 << 4),
        0xdeadbeef
    }};

    DecodeAllAndApply<std::tuple<uint32_t, uint64_t, WriteableBuffer>>{}(cmd_block, DoStuff);
}
