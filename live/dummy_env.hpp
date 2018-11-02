#pragma once

#include <cstdint>

/*********************
 * DUMMY ENVIRONMENT *
 *********************/
struct CmdBlock {
    uint32_t data[16];

    uint32_t ReadU32(size_t off) const { return data[off]; }
    void WriteU32(size_t off, uint32_t val) { data[off] = val; }
};


struct WriteableBuffer {
    uint32_t address;
    uint32_t size;
};
std::ostream& operator<<(std::ostream& os, const WriteableBuffer& buf) {
    os << "WriteableBuffer @ 0x" << std::hex << buf.address << " (" << buf.size << " bytes)";
    return os;
}


struct FileDescriptor {
    uint32_t fd;
};

std::ostream& operator<<(std::ostream& os, const FileDescriptor& fd) {
    os << "FileDescriptor { " << fd.fd << " }";
    return os;
}


bool IsValidBufferDescriptor(uint32_t descriptor) {
    return ((descriptor & 0x8) != 0);
}

std::pair<uint32_t, uint32_t> DecodeBufferDescriptor(uint32_t descriptor) {
    return { descriptor >> 4 /* size */,
             descriptor & 0xf /* flags */ };
}

