#pragma once

#include <cstdint>
#include <tuple>

template<uint32_t Index, typename RequestNormalList, typename RequestSpecialList, typename ResponseNormalList, typename ResponseSpecialList = std::tuple<>>
struct IPCCmdBase {
    using request_list = decltype(std::tuple_cat(RequestNormalList{}, RequestSpecialList{}));
    // The response list has an implicit uint32_t for the result code
    using response_list = decltype(std::tuple_cat(std::tuple<uint32_t>{}, ResponseNormalList{}, ResponseSpecialList{}));

    static constexpr uint32_t request_num_normals = std::tuple_size_v<RequestNormalList>;
    static constexpr uint32_t request_num_specials = std::tuple_size_v<RequestSpecialList>;
    static constexpr uint32_t response_num_normals = std::tuple_size_v<ResponseNormalList>;
    static constexpr uint32_t response_num_specials = std::tuple_size_v<ResponseSpecialList>;

    static constexpr uint32_t request_header = (Index << 16) | (request_num_specials << 8) | request_num_normals;
    static constexpr uint32_t response_header = (Index << 16) | (response_num_specials << 8) | response_num_normals;
};

template<uint32_t CommandIndex>
struct IPCCmd {
    template<typename... RequestNormals>
    struct normal {
        template<typename... RequestSpecials>
        struct special {
            template<typename... ResponseNormals>
            struct response {
                using fin = IPCCmdBase<CommandIndex, std::tuple<RequestNormals...>, std::tuple<RequestSpecials...>, std::tuple<ResponseNormals...>>;
            };
        };
    };
};

template<typename IPCCmd>
struct IPCDataBaseEntry {
    using Cmd = IPCCmd;
    const char* name;
};

template<typename... Commands>
using IPCCmdDatabase = std::tuple<IPCDataBaseEntry<Commands>...>;
