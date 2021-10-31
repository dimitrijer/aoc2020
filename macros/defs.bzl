load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
)

def spec_test(name, deps, srcs):
    haskell_test(
        name = name,
        srcs = srcs + ["Spec.hs"],
        main_function = "Spec.main",
        tools = ["@hspec-discover//:bin"],
        compiler_flags = ["-DHSPEC_DISCOVER=$(execpath @hspec-discover//:bin)"],
        deps = deps + ["//test:hspec"],
        tags = ["test"],
        visibility = [
            "//visibility:public",
        ],
    )
