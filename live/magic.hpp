template<typename F, typename... Ts>
struct CallWithSequentialEvaluation {
    using Result = std::invoke_result_t<F, Ts...>;
    Result result;

    CallWithSequentialEvaluation(F&& f, Ts&&... ts) : result(f(std::forward<Ts>(ts)...)) {
    }

    operator Result() && {
        return result;
    }
};
