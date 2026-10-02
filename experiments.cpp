#include <iostream>
#include <cmath>
#include <vector>
#include <utility>
#include <algorithm>
#include <iterator>
#include <ranges>


namespace AD {
    template <typename ID>
    struct Var;

    template <typename ID, typename... Ts>
    constexpr float get_val(Var<ID> v, Ts&&...) {
        return v.val;
    }

    template <typename ID, typename T, typename... Ts>
    constexpr float get_val(T&&, Ts&&... rest) {
        return get_val<ID>(std::forward<Ts>(rest)...);
    }



    template <typename ID>
    struct Var {

        float val{};

        constexpr Var() {}
        explicit constexpr Var(float v) : val(v) {}
        constexpr void operator = (float v) { val = v; }

        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return get_val<ID>(std::forward<Args>(args)...);
        }
    };



    template <float V>
    struct Const {
        [[nodiscard]] static constexpr float eval(auto&&...) { return V; }
    };

    template <float V>
    constexpr auto make_const() {
        return Const<V>{};
    }



    template <typename L, typename R>
    struct Add {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return L::eval(std::forward<Args>(args)...) + R::eval(std::forward<Args>(args)...);
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename L, typename R>
    struct Mul {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return L::eval(std::forward<Args>(args)...) * R::eval(std::forward<Args>(args)...);
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename E>
    struct Neg {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return -E::eval(std::forward<Args>(args)...);
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename E>
    struct Sin {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return std::sin(E::eval(std::forward<Args>(args)...));
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename E>
    struct Cos {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return std::cos(E::eval(std::forward<Args>(args)...));
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename E>
    struct Exp {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return std::exp(E::eval(std::forward<Args>(args)...));
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename L, typename R>
    struct Div {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return L::eval(std::forward<Args>(args)...) / R::eval(std::forward<Args>(args)...);
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };

    template <typename E>
    struct Log {
        template <typename... Args>
        [[nodiscard]] static constexpr float eval(Args&&... args) {
            return std::log(E::eval(std::forward<Args>(args)...));
        }
        template <typename... Args> constexpr float operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
    };



    template <typename A, typename B>
    using Sub = Add<A, Neg<B>>;





    // takes f(var, ...) and turns it into f(new_var, ...)
    // maybe I should do it with structs and using type = ... inside them? Maybe...
    template<template<typename...> typename E, typename... Children, typename Var, typename NewVar>
    constexpr auto substitute(E<Children...> expr, Var var, NewVar new_var) {
        if constexpr (std::is_same_v<E<Children...>, Var>) {
            return new_var;
        } else {
            return E<decltype(substitute(Children{}, var, new_var))...>{};
        }
    }

    template<typename Expr, typename Var, typename NewVar>
    requires(!std::is_same_v<Expr, Var>)
    constexpr auto substitute(Expr expr, Var, NewVar) {
        return expr;
    }







    template <typename E, typename WRT>
    struct Derivative;

    template <typename ID1, typename ID2>
    struct Derivative<Var<ID1>, Var<ID2>> {
        using type = Const<std::is_same_v<ID1, ID2> ? 1.0f : 0.0f>;
    };

    template <float V, typename WRT>
    struct Derivative<Const<V>, WRT> {
        using type = Const<0.0f>;
    };

    template <typename L, typename R, typename WRT>
    struct Derivative<Add<L, R>, WRT> {
        using type = Add<typename Derivative<L, WRT>::type, typename Derivative<R, WRT>::type>;
    };

    template <typename L, typename R, typename WRT>
    struct Derivative<Mul<L, R>, WRT> {
        using type = Add<Mul<typename Derivative<L, WRT>::type, R>, Mul<L, typename Derivative<R, WRT>::type>>;
    };

    template <typename E, typename WRT>
    struct Derivative<Neg<E>, WRT> {
        using type = Neg<typename Derivative<E, WRT>::type>;
    };

    template <typename E, typename WRT>
    struct Derivative<Sin<E>, WRT> {
        using type = Mul<Cos<E>, typename Derivative<E, WRT>::type>;
    };

    template <typename E, typename WRT>
    struct Derivative<Cos<E>, WRT> {
        using type = Neg<Mul<Sin<E>, typename Derivative<E, WRT>::type>>;
    };

    template <typename E, typename WRT>
    struct Derivative<Exp<E>, WRT> {
        using type = Mul<Exp<E>, typename Derivative<E, WRT>::type>;
    };

    template <typename L, typename R, typename WRT>
    struct Derivative<Div<L, R>, WRT> {
        using dL = typename Derivative<L, WRT>::type;
        using dR = typename Derivative<R, WRT>::type;
        using type = Div<Sub<Mul<dL, R>, Mul<L, dR>>, Mul<R, R>>;
    };

    template <typename E, typename WRT>
    struct Derivative<Log<E>, WRT> {
        using type = Div<typename Derivative<E, WRT>::type, E>;
    };



    template <typename E, typename WRT>
    constexpr auto make_derivative_function(E&&, WRT&&) {
        using derivative = Derivative<std::remove_cvref_t<E>, std::remove_cvref_t<WRT>>::type;
        return derivative{};
    }




    constexpr auto operator + (auto&& e1, auto&& e2) {
        return Add<std::remove_cvref_t<decltype(e1)>, std::remove_cvref_t<decltype(e2)>>{};
    }

    constexpr auto operator - (auto&& e1, auto&& e2) {
        return Sub<std::remove_cvref_t<decltype(e1)>, std::remove_cvref_t<decltype(e2)>>{};
    }

    constexpr auto operator * (auto&& e1, auto&& e2) {
        return Mul<std::remove_cvref_t<decltype(e1)>, std::remove_cvref_t<decltype(e2)>>{};
    }

    constexpr auto operator / (auto&& e1, auto&& e2) {
        return Div<std::remove_cvref_t<decltype(e1)>, std::remove_cvref_t<decltype(e2)>>{};
    }

    constexpr auto log(auto&& e) {
        return Log<std::remove_cvref_t<decltype(e)>>{};
    }

    constexpr auto sin(auto&& e) {
        return Sin<std::remove_cvref_t<decltype(e)>>{};
    }

    constexpr auto cos(auto&& e) {
        return Cos<std::remove_cvref_t<decltype(e)>>{};
    }

    constexpr auto exp(auto&& e) {
        return Exp<std::remove_cvref_t<decltype(e)>>{};
    }


    constexpr std::size_t factorial(std::size_t n) {
        return n == 0 ? 1 : n * factorial(n - 1);
    }

    template <std::size_t N>
    constexpr auto pow(auto f) {
        if constexpr (N == 0) return Const<1.0f>{};
        else return f * pow<N - 1>(f);
    }

    template <std::size_t K>
    constexpr auto make_derivatives(auto f, auto x) {
        if constexpr (K == 0) {
            return std::tuple{f};
        } else {
            auto prev = make_derivatives<K - 1>(f, x);
            auto last = std::get<K - 1>(prev);
            auto next = make_derivative_function(last, x);
            return std::tuple_cat(prev, std::tuple{next});
        }
    }

};


// using a lambda there will give a new type every time the macro is expanded
#define make_var(x) AD::Var<decltype([]{})>{x}
#define define_var() AD::Var<decltype([]{})>



using Vec = std::vector<float>;

auto lerp(auto a, auto b, auto t) {
    return a + (b - a) * t;
}

auto inv_lerp(auto a, auto b, auto c) {
    return (c - a) / (b - a);
}

// integrating

struct ForwardEuler {
    static Vec step(auto f, float t, Vec y, float dt) {
        auto f_val = f(t, y);
        for (std::size_t i = 0; i < y.size(); ++i) {
            y[i] += f_val[i] * dt;
        }

        return y;
    }
};

struct RK4 {
    static Vec step(auto f, float t, Vec y, float dt) {
        auto k1 = f(t, y);

        Vec y2{}; y2.reserve(y.size());
        for (std::size_t i = 0; i < y.size(); ++i) {
            y2.push_back(y[i] + 0.5f * dt * k1[i]);
        }

        auto k2 = f(t + 0.5f * dt, std::move(y2));

        Vec y3{}; y3.reserve(y.size());
        for (std::size_t i = 0; i < y.size(); ++i) {
            y3.push_back(y[i] + 0.5f * dt * k2[i]);
        }

        auto k3 = f(t + 0.5f * dt, std::move(y3));

        Vec y4{}; y4.reserve(y.size());
        for (std::size_t i = 0; i < y.size(); ++i) {
            y4.push_back(y[i] + dt * k3[i]);
        }

        auto k4 = f(t + dt, std::move(y4));

        for (std::size_t i = 0; i < y.size(); ++i) {
            y[i] += dt / 6.0f * (k1[i] + 2.0f * k2[i] + 2.0f * k3[i] + k4[i]);
        }

        return y;
    }
};

struct SSPRK3 {
    static Vec step(auto f, float t, Vec u0, float dt) {
        // u1 = u0 + dt f(t, u0)
        auto u1 = ForwardEuler::step(f, t, u0, dt);

        // u2 = 3/4 u0 + 1/4 (u1 + dt f(t+dt, u1))
        auto temp = ForwardEuler::step(f, t + dt, std::move(u1), dt);

        // temp -> u2
        for (std::size_t i = 0; i < u0.size(); ++i)
            temp[i] = 0.75f * u0[i] + 0.25f * temp[i];

        // u3 = 1/3 u0 + 2/3 (u2 + dt f(t+dt/2, u2))
        temp = ForwardEuler::step(f, t + 0.5f * dt, std::move(temp), dt);

        // u0 -> u3
        for (std::size_t i = 0; i < u0.size(); ++i) {
            u0[i] = (1.0f / 3.0f) * u0[i] + (2.0f / 3.0f) * temp[i];
        }

        return u0;
    }
};

// ts_sol needs to be sorted. if ts_sol[i] \notin [t0, final_t], it's ignored.
// ts_sol[i]'s output is linearly interpolated if it isn't exactly a grid point
template <typename IntegrationStep = RK4>
auto integrate(auto f, Vec y0, float t0, float final_t, float step = 1e-3, Vec ts_sol = Vec{}) -> std::pair<Vec, std::vector<Vec>> {
    bool save_all = ts_sol.size() == 0;

    std::size_t sols_size = save_all ? static_cast<std::size_t>((final_t - t0) / step) + 1 : ts_sol.size();
    std::vector<Vec> sols;
    if (!save_all) {
        sols.resize(sols_size);
        for (std::size_t i = 0; i < sols_size; ++i) sols[i].reserve(y0.size());
    } else {
        ts_sol.reserve(sols_size);
        sols.reserve(sols_size);
    }

    std::size_t curr_sol_idx = save_all ? 0 : std::distance(ts_sol.begin(), std::find_if(ts_sol.begin(), ts_sol.end(), [t0](float t_sol) { return t_sol >= t0; }));
    for (float t_last = t0, t = std::min(t0 + step, final_t); t_last < final_t && curr_sol_idx < sols_size; t_last = std::exchange(t, std::min(t + step, final_t))) {
        
        float dt = t - t_last;

        if (!save_all && ts_sol[curr_sol_idx] >= t_last && ts_sol[curr_sol_idx] <= t) {

            auto it = std::find_if_not(
                std::next(ts_sol.begin(), curr_sol_idx + 1), 
                ts_sol.end(), 
                [t_last, t](float t_sol) { return t_sol >= t_last && t_sol <= t; }
            );
            std::size_t next_sol_idx = std::distance(ts_sol.begin(), it);

            auto last_y = y0;
            y0 = IntegrationStep::step(f, t_last, std::move(y0), dt);

            for (std::size_t i = 0; i < y0.size(); ++i) {
                for (std::size_t j = curr_sol_idx; j < next_sol_idx; ++j) {
                    sols[j].push_back(lerp(last_y[i], y0[i], inv_lerp(t_last, t, ts_sol[j])));
                }
            }

            curr_sol_idx = next_sol_idx;
        } else {
            y0 = IntegrationStep::step(f, t_last, std::move(y0), dt);

            if (save_all) {
                sols.push_back(y0);
                ts_sol.push_back(t);
            }
        }
    }

    return { ts_sol, sols };
}




// samplers (this is really ugly)

enum class Side {
    left,
    right
};

template <typename Left, typename Right>
struct Sampler {
    template <typename Container>
    static auto sample(const Container& container, std::ptrdiff_t idx, auto&&... args) {
        if (idx >= 0 && idx < std::ssize(container)) return container[idx];
        if (idx < 0) return Left::template sample<Side::left>(container, idx, args...);
        return Right::template sample<Side::right>(container, idx, args...);
    }
};

template <auto T>
struct SampleConst {
    template <Side side, typename Container>
    static auto sample(const Container&, std::ptrdiff_t, auto&&...) {
        return static_cast<std::ranges::range_value_t<Container>>(T);
    }
};

struct SampleExtend {
    template <Side side, typename Container>
    static auto sample(const Container& container, std::ptrdiff_t, auto&&...) {
        if constexpr (side == Side::left) return container.front();
        else return container.back();
    }
};

template <typename Func>
struct SampleFunctional {
    template <Side side, typename Container, typename... Args>
    static auto sample(const Container&, std::ptrdiff_t, Args&&... args) {
        return Func{}(std::forward<Args>(args)...); // ugh
    }
};




// reconstruction

float minmod(float a, float b) {
    if (a * b <= 0.0f) return 0.0f;

    if (std::abs(a) < std::abs(b)) return a;
    return b;
}

float maxmod(float a, float b) {
    if (a * b <= 0.0f) return 0.0f;

    if (std::abs(a) < std::abs(b)) return b;
    return a;
}

struct SuperBee {

    template <typename SampleStrategy>
    static std::pair<float, float> at_interface(const Vec& U, std::ptrdiff_t i, float t) {
        // interface i is between U[i] and U[i + 1]

        auto cell = [&](std::ptrdiff_t idx) { return SampleStrategy::sample(U, idx, t); };
        
        float dx = 1.0f; // can be whatever, since after computing sigma (that divides by dx), we multiply by dx to compute SL and SR

        float sigma1 = minmod((cell(i + 1) - cell(i)) / dx, 2.0f * (cell(i) - cell(i - 1)) / dx);
        float sigma2 = minmod(2.0f * (cell(i + 1) - cell(i)) / dx, (cell(i) - cell(i - 1)) / dx);
        float sigma_L = maxmod(sigma1, sigma2);

        sigma1 = minmod((cell(i + 2) - cell(i + 1)) / dx, 2.0f * (cell(i + 1) - cell(i)) / dx);
        sigma2 = minmod(2.0f * (cell(i + 2) - cell(i + 1)) / dx, (cell(i + 1) - cell(i)) / dx);
        float sigma_R = maxmod(sigma1, sigma2);

        // get right interface of U[i]
        float SL = cell(i) + sigma_L * dx * 0.5f; // dx * 0.5 = (x_{i+1/2} - x_i)
        float SR = cell(i + 1) + sigma_R * dx * -0.5f; // dx * -0.5 = (x_{i+1-1/2} - x_{i+1})

        return { SL, SR };
    }

};



// numerical fluxes

struct Godunov {
    static float compute(auto f, auto, float SL, float) {
        return f(SL); // simplification for my flux
    }
};


template <typename Reconstructor, typename NumericalFlux, typename SampleStrategy>
auto finite_volume(auto physical_flux, auto flux_derivative, float dx) {
    return [=](float t, const Vec& S) {

        auto flux_at_interface = [&](std::ptrdiff_t i) {
            auto [SL, SR] = Reconstructor::template at_interface<SampleStrategy>(S, i, t);
            return NumericalFlux::compute(physical_flux, flux_derivative, SL, SR);
        };

        Vec ds; ds.reserve(S.size());

        float F_last = flux_at_interface(-1);
        for (std::size_t i = 0; i < S.size(); ++i) {
            float F_next = flux_at_interface(static_cast<std::ptrdiff_t>(i));
            ds.push_back(-(F_next - F_last) / dx);
            F_last = F_next;
        }

        return ds;
    };
}














static constexpr auto mu_w = AD::Const<1e-3f>{};
static constexpr auto mu_g = AD::Const<1e-5f>{};
static constexpr auto S_wc = AD::Const<0.2f>{};
static constexpr auto S_gr = AD::Const<0.1f>{};

static constexpr auto S_inicial = 0.2f;
static constexpr auto S_injecao = 0.9f;



constexpr auto S_n(auto S) {
    return (S - S_wc) / (AD::Const<1.0f>{} - S_wc - S_gr);
}

constexpr auto k_rw(auto S) {
    const auto s = S_n(S);
    return s * s * s * s;
}

constexpr auto k_rg(auto S) {
    const auto s = S_n(S);
    return (AD::Const<1.0f>{} - s) * (AD::Const<1.0f>{} - s);
}

constexpr auto f(auto S) {
    auto lambda_w = k_rw(S) / mu_w;
    auto lambda_g = k_rg(S) / mu_g;
    return lambda_w / (lambda_w + lambda_g);
}



int main() {

    // geração da malha
    std::size_t N = 500;
    Vec x; x.reserve(N);
    for (std::size_t i = 0; i < N; ++i) {
        x.push_back((static_cast<float>(i) + 0.5f) / static_cast<float>(N));
    }
    float dx = 1.0f / static_cast<float>(N);

    Vec y0(N, S_inicial); // initial condition


    using S_var = define_var();
    constexpr auto flux = f(S_var{});
    constexpr auto flux_der = make_derivative_function(flux, S_var{});

    const float CFL = 1.0f;
    float max_f_der = flux_der(S_var{x[0]});
    for (std::size_t i = 1; i < x.size(); ++i) {
        max_f_der = std::max(max_f_der, flux_der(S_var{x[i]}));
    }

    float dt = CFL * dx / max_f_der;
    std::cout << "!dt: " << dt << "\n";



    auto rhs = finite_volume<SuperBee, Godunov, Sampler<SampleConst<S_injecao>, SampleExtend>>(
        [flux](float S) constexpr { return flux(S_var{S}); },
        [flux_der](float S) constexpr { return flux_der(S_var{S}); },
        dx
    );

    std::vector ts_sol{0.0f, 0.15f, 0.4f, 0.65f};

    auto res = integrate<SSPRK3>(rhs, y0, 0.0f, 0.70f, dt, ts_sol).second;

    for (std::size_t i = 0; i < res.size(); ++i) {
        std::cout << "begin trace\n";
        std::cout << "marker: o\n";

        for (std::size_t j = 0; j < x.size(); ++j) {
            std::cout << "point: " << x[j] << ", " << res[i][j] << "\n";
        }

        std::cout << "end trace\n";
    }

    return 0;
}
