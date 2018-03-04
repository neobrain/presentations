#include <tuple>
#include <iostream>
#include <iomanip>

struct CPU {
	uint32_t reg[16];
};

struct Process {};
struct DmaObject {};

using VAddr = uint32_t;

std::tuple<uint32_t, DmaObject*>
DoStartDma(Process* dst_process, VAddr dst_addr,
           Process* src_process, VAddr src_addr, uint32_t data_size) {
	auto dma_object = new DmaObject;
	std::cout << "DoStartDma:"
		<< " dst_process=" << dst_process
		<< ", dst_addr="    << dst_addr
		<< ", src_process=" << src_process
		<< ", src_addr="    << src_addr
		<< ", data_size="   << data_size
		<< "\n            returning "   << src_addr * dst_addr
		<< " and "   << dma_object
		<< std::endl;
	return std::make_tuple(uint32_t{src_addr * dst_addr}, dma_object);
}

template<typename T>
T FromRegister(uint32_t value) {
	std::cout << "Decoding value " << value << '\n';
	if constexpr (!std::is_pointer_v<T>) {
		return static_cast<T>(value);
	} else {
		// Just hack together some pointer based on the given value
		return reinterpret_cast<T>(static_cast<uintptr_t>(value));
	}
}

template<typename T>
uint32_t ToRegister(T value) {
	std::cout << "Encoding value " << value << '\n';
	if constexpr (!std::is_pointer_v<T>) {
		return static_cast<uint32_t>(value);
	} else {
		// Just return the raw object address as a value
		return static_cast<uint32_t>(reinterpret_cast<uintptr_t>(value));
	}
}

////////////////////
// FUNCTION TRAITS
template<typename F>
struct FunctionTraits;

template<typename Ret, typename... Args>
struct FunctionTraits<Ret(*)(Args...)> {
  using InArgs = std::tuple<Args...>;
  using OutArgs = Ret;
};

////////////////////
// DECODER
template<typename ArgsTuple>
struct Decoder;

template<typename... Args>
struct Decoder<std::tuple<Args...>> {
	static std::tuple<Args...> Decode(CPU& cpu) {
		size_t reg_index = 0;
		return std::tuple<Args... > {
				FromRegister<Args>(cpu.reg[reg_index++])
				...
		};
	}
};

////////////////////
// ENCODER
template<typename ArgsTuple>
struct Encoder;

template<typename... Args>
struct Encoder<std::tuple<Args...>> {
	CPU& cpu;

	auto operator()(Args... args) {
		size_t reg_index = 0;
		((cpu.reg[reg_index++] = ToRegister<Args>(args)), ...);
	}
};

template<auto SVCImpl>
void WrapSVCImpl(CPU& cpu) {
  using InArgs = typename FunctionTraits<decltype(SVCImpl)>::InArgs;
  using OutArgs = typename FunctionTraits<decltype(SVCImpl)>::OutArgs;

  auto args = Decoder<InArgs>::Decode(cpu);

  auto result = std::apply(SVCImpl, args);

  std::apply(Encoder<OutArgs>{cpu}, result);
}

void HandleSVC(CPU& cpu, uint32_t index) {
	switch (index) {
	case 0x55:
		WrapSVCImpl<DoStartDma>(cpu);
		break;
	}
}

int main() {
	CPU cpu {{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }};
	HandleSVC(cpu, 0x55);

	std::cout << '\n';
	std::cout << "CPU registers on return: ";
	for (auto reg : cpu.reg)
		std::cout << "0x" << std::hex << reg << ' ';
	std::cout << std::endl;
}
