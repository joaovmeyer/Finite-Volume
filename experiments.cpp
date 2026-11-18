#include <iostream>
#include <cmath>
#include <vector>
#include <utility>
#include <algorithm>
#include <iterator>
#include <ranges>
#include <fstream>


namespace AD {
	template <typename ID>
	struct Var;

	template <typename ID, typename... Ts>
	constexpr double get_val(Var<ID> v, Ts&&...) {
		return v.val;
	}

	template <typename ID, typename T, typename... Ts>
	constexpr double get_val(T&&, Ts&&... rest) {
		return get_val<ID>(std::forward<Ts>(rest)...);
	}



	template <typename ID>
	struct Var {

		double val{};

		constexpr Var() {}
		explicit constexpr Var(double v) : val(v) {}
		constexpr void operator = (double v) { val = v; }

		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return get_val<ID>(std::forward<Args>(args)...);
		}
	};



	template <double V>
	struct Const {
		[[nodiscard]] static constexpr double eval(auto&&...) { return V; }
		[[nodiscard]] constexpr double val() const {return V; }
	};

	template <double V>
	constexpr auto make_const() {
		return Const<V>{};
	}



	template <typename L, typename R>
	struct Add {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return L::eval(std::forward<Args>(args)...) + R::eval(std::forward<Args>(args)...);
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename L, typename R>
	struct Mul {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return L::eval(std::forward<Args>(args)...) * R::eval(std::forward<Args>(args)...);
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename E>
	struct Neg {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return -E::eval(std::forward<Args>(args)...);
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename E>
	struct Sin {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return std::sin(E::eval(std::forward<Args>(args)...));
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename E>
	struct Cos {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return std::cos(E::eval(std::forward<Args>(args)...));
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename E>
	struct Exp {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return std::exp(E::eval(std::forward<Args>(args)...));
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename L, typename R>
	struct Div {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return L::eval(std::forward<Args>(args)...) / R::eval(std::forward<Args>(args)...);
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
	};

	template <typename E>
	struct Log {
		template <typename... Args>
		[[nodiscard]] static constexpr double eval(Args&&... args) {
			return std::log(E::eval(std::forward<Args>(args)...));
		}
		template <typename... Args> constexpr double operator()(Args&&... args) const { return eval(std::forward<Args>(args)...); }
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
		using type = Const<std::is_same_v<ID1, ID2> ? 1.0 : 0.0>;
	};

	template <double V, typename WRT>
	struct Derivative<Const<V>, WRT> {
		using type = Const<0.0>;
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
		if constexpr (N == 0) return Const<1.0>{};
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



using Vec = std::vector<double>;

auto lerp(auto a, auto b, auto t) {
	return a + (b - a) * t;
}

auto inv_lerp(auto a, auto b, auto c) {
	return (c - a) / (b - a);
}

// integrating

struct ForwardEuler {
	static Vec step(auto f, double t, Vec y, double dt) {
		auto f_val = f(t, y);
		for (std::size_t i = 0; i < y.size(); ++i) {
			y[i] += f_val[i] * dt;
		}

		return y;
	}
};

struct RK4 {
	static Vec step(auto f, double t, Vec y, double dt) {
		auto k1 = f(t, y);

		Vec y2{}; y2.reserve(y.size());
		for (std::size_t i = 0; i < y.size(); ++i) {
			y2.push_back(y[i] + 0.5 * dt * k1[i]);
		}

		auto k2 = f(t + 0.5 * dt, std::move(y2));

		Vec y3{}; y3.reserve(y.size());
		for (std::size_t i = 0; i < y.size(); ++i) {
			y3.push_back(y[i] + 0.5 * dt * k2[i]);
		}

		auto k3 = f(t + 0.5 * dt, std::move(y3));

		Vec y4{}; y4.reserve(y.size());
		for (std::size_t i = 0; i < y.size(); ++i) {
			y4.push_back(y[i] + dt * k3[i]);
		}

		auto k4 = f(t + dt, std::move(y4));

		for (std::size_t i = 0; i < y.size(); ++i) {
			y[i] += dt / 6.0 * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
		}

		return y;
	}
};

struct SSPRK3 {
	static Vec step(auto f, double t, Vec u0, double dt) {
		// u1 = u0 + dt f(t, u0)
		auto u1 = ForwardEuler::step(f, t, u0, dt);

		// u2 = 3/4 u0 + 1/4 (u1 + dt f(t+dt, u1))
		auto temp = ForwardEuler::step(f, t + dt, std::move(u1), dt);

		// temp -> u2
		for (std::size_t i = 0; i < u0.size(); ++i)
			temp[i] = 0.75 * u0[i] + 0.25 * temp[i];

		// u3 = 1/3 u0 + 2/3 (u2 + dt f(t+dt/2, u2))
		temp = ForwardEuler::step(f, t + 0.5 * dt, std::move(temp), dt);

		// u0 -> u3
		for (std::size_t i = 0; i < u0.size(); ++i) {
			u0[i] = (1.0 / 3.0) * u0[i] + (2.0 / 3.0) * temp[i];
		}

		return u0;
	}
};

// ts_sol needs to be sorted. if ts_sol[i] \notin (t0, final_t], it's ignored.
// ts_sol[i]'s output is linearly interpolated if it isn't exactly a grid point
template <typename IntegrationStep = RK4>
auto integrate(auto f, Vec y0, double t0, double final_t, double step = 1e-3, Vec ts_sol = Vec{}) -> std::pair<Vec, std::vector<Vec>> {
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

	std::size_t curr_sol_idx = save_all ? 0 : std::distance(ts_sol.begin(), std::find_if(ts_sol.begin(), ts_sol.end(), [t0](double t_sol) { return t_sol >= t0; }));
	for (double t_last = t0, t = std::min(t0 + step, final_t); t_last < final_t && curr_sol_idx < sols_size; t_last = std::exchange(t, std::min(t + step, final_t))) {
		
		double dt = t - t_last;

		if (!save_all && ts_sol[curr_sol_idx] >= t_last && ts_sol[curr_sol_idx] <= t) {

			auto it = std::find_if_not(
				std::next(ts_sol.begin(), curr_sol_idx + 1), 
				ts_sol.end(), 
				[t_last, t](double t_sol) { return t_sol >= t_last && t_sol <= t; }
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

double minmod(double a, double b) {
	if (a * b <= 0.0) return 0.0;

	if (std::abs(a) < std::abs(b)) return a;
	return b;
}

double maxmod(double a, double b) {
	if (a * b <= 0.0) return 0.0;

	if (std::abs(a) < std::abs(b)) return b;
	return a;
}


// considers dx to be always 1. Not a problem for now
template <typename SlopeLimiter>
struct PiecewiseLinear {

	template <typename SampleStrategy>
	static std::pair<double, double> at_interface(const Vec& U, std::ptrdiff_t i, double t) {

		auto cell = [&](std::ptrdiff_t idx) { return SampleStrategy::sample(U, idx, t); };

		auto sigma_L = SlopeLimiter::get_slope(cell, i);
		auto sigma_R = SlopeLimiter::get_slope(cell, i + 1);

		double UL = cell(i) + sigma_L * 0.5;
		double UR = cell(i + 1) - sigma_R * 0.5;

		return { UL, UR };
	}

};



// slope limiters

struct Minmod {
	static double get_slope(auto cell, std::ptrdiff_t i) {
		return minmod(cell(i + 1) - cell(i), cell(i) - cell(i - 1));
	}
};

struct MonotonizedCentral {
	static double get_slope(auto cell, std::ptrdiff_t i) {
		return minmod(0.5 * (cell(i + 1) - cell(i - 1)), 2.0 * minmod(cell(i + 1) - cell(i), cell(i) - cell(i - 1)));
	}
};

struct Superbee {
	static double get_slope(auto cell, std::ptrdiff_t i) {
		double sigma1 = minmod(cell(i + 1) - cell(i), 2.0 * (cell(i) - cell(i - 1)));
		double sigma2 = minmod(2.0 * (cell(i + 1) - cell(i)), cell(i) - cell(i - 1));
		return maxmod(sigma1, sigma2);
	}
};



// numerical fluxes

struct Godunov {
	static double compute(auto f, auto, double SL, double) {
		return f(SL); // simplification for my flux
	}
};


template <typename Reconstructor, typename NumericalFlux, typename SampleStrategy>
auto finite_volume(auto physical_flux, auto flux_derivative, double dx) {
	return [=](double t, const Vec& S) {

		auto flux_at_interface = [&](std::ptrdiff_t i) {
			auto [SL, SR] = Reconstructor::template at_interface<SampleStrategy>(S, i, t);
			return NumericalFlux::compute(physical_flux, flux_derivative, SL, SR);
		};

		Vec ds; ds.reserve(S.size());

		double F_last = flux_at_interface(-1);
		for (std::size_t i = 0; i < S.size(); ++i) {
			double F_next = flux_at_interface(static_cast<std::ptrdiff_t>(i));
			ds.push_back(-(F_next - F_last) / dx);
			F_last = F_next;
		}

		return ds;
	};
}














static constexpr auto mu_w = AD::Const<0.1>{};
static constexpr auto mu_g = AD::Const<0.5>{};
static constexpr auto S_wc = AD::Const<0.2>{};
static constexpr auto S_gr = AD::Const<0.1>{};

static constexpr auto S_inicial = 0.6;
static constexpr auto S_injecao = 0.9;




constexpr auto S_n(auto S) {
	return (S - S_wc) / (AD::Const<1.0>{} - S_wc - S_gr);
}

constexpr auto k_rw(auto S) {
	const auto s = S_n(S);
	return s * s * s * s;
}

constexpr auto k_rg(auto S) {
	const auto s = S_n(S);
	return (AD::Const<1.0>{} - s) * (AD::Const<1.0>{} - s);
}

constexpr auto f(auto S) {
	auto lambda_w = k_rw(S) / mu_w;
	auto lambda_g = k_rg(S) / mu_g;
	return lambda_w / (lambda_w + lambda_g);
}



int main() {

	using S_var = define_var();
	constexpr auto flux = f(S_var{});
	constexpr auto flux_der = make_derivative_function(flux, S_var{});
	constexpr auto flux_der_second = make_derivative_function(flux_der, S_var{});
	
	auto S0 = [](double x) { return S_inicial; };
	auto S0_der = [](double x) { return 0.0; };


	// geração da malha
	std::size_t N = 1000;
	Vec xs; xs.reserve(N);
	for (std::size_t i = 0; i < N; ++i) {
		xs.push_back((static_cast<double>(i) + 0.5) / static_cast<double>(N));
	}
	double dx = 1.0 / static_cast<double>(N);

	Vec y0; y0.reserve(N); // initial condition
	for (std::size_t i = 0; i < N; ++i) {
		y0.push_back(S0(xs[i]));
	}


	// find first shock time
	double min_t = std::numeric_limits<double>::infinity();
	double min_x{};
	for (std::size_t i = 0; i < xs.size(); ++i) {
		double den = flux_der_second(S_var{S0(xs[i])}) * S0_der(xs[i]);
		if (den >= 0.0) continue;
		double t_now = -1.0 / den;
		if (t_now < min_t) {
			min_t = t_now;
			min_x = xs[i] + flux_der(S_var{S0(xs[i])}) * min_t;
		}
	}

	std::cout << "!min_t: " << min_t << ", min_x: " << min_x << "\n";


	// plain bissection on the second derivative would have done it, but this is simpler
	double max_f_der = flux_der(S_var{S_wc.val()});
	for (std::size_t i = 1; i <= 10000; ++i) {
		max_f_der = std::max(max_f_der, flux_der(S_var{S_wc.val() + (1.0 - S_gr.val() - S_wc.val()) * static_cast<double>(i) / 10000.0}));
	}

	const double CFL = 0.5;
	double dt = CFL * dx / max_f_der;
	std::cout << "!dt: " << dt << "\n";



	auto rhs = finite_volume<PiecewiseLinear<MonotonizedCentral>, Godunov, Sampler<SampleConst<S_injecao>, SampleExtend>>(
		[flux](double S) constexpr { return flux(S_var{S}); },
		[flux_der](double S) constexpr { return flux_der(S_var{S}); },
		dx
	);


	double T_max = 1.0;

	// std::vector ts_sol{0.25, 0.5, 0.75, 1.0, min_t}; std::ranges::sort(ts_sol);
	std::size_t NT = 1000;
	Vec ts_sol; ts_sol.reserve(NT);
	for (std::size_t i = 0; i < NT; ++i) {
		ts_sol.push_back((static_cast<double>(i) + 0.5) / static_cast<double>(NT) * T_max);
	}

	auto res = integrate<SSPRK3>(rhs, y0, 0.0, T_max, dt, ts_sol).second;


	std::ofstream out_file("../../random python files/GAN IC/experimentos/referencias/caso3.txt");
	if (out_file.is_open()) {
		
		for (const auto& x : xs) {
			out_file << x << ",";
		}
		out_file << "\n";

		for (const auto& t : ts_sol) {
			out_file << t << ",";
		}
		out_file << "\n";

		for (const auto& sol_slice : res) {
			for (const auto& val : sol_slice) {
				out_file << val << ",";
			}
		}

		out_file.close(); 
	} else {
		return 1;
	}

	/*for (std::size_t i = 0; i < res.size(); ++i) {
		std::cout << "begin trace\n";
		std::cout << "marker: o\n";

		for (std::size_t j = 0; j < xs.size(); ++j) {
			std::cout << "point: " << xs[j] << ", " << res[i][j] << "\n";
		}

		std::cout << "end trace\n";
	}

	std::cout << "begin trace\n";
	std::cout << "marker: o\n";
	std::cout << "point: " << min_x + flux_der(S_var{S0(min_x)}) * min_t << ", " << S0(min_x) << "\n";
	std::cout << "end trace\n";*/

	return 0;
}
