#include <type_traits>

template<typename F, typename... Ts>
struct CallWithSequentialEvaluation {
    using Result = std::invoke_result_t<F, Ts...>;
    Result result;

    CallWithSequentialEvaluation(F&& f, Ts&&... ts) : result(f(std::forward<Ts>(ts)...)) {
    }

    decltype(auto) get() && {
        return std::move(result);
    }
};
