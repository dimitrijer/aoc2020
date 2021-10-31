workspace(name = "aoc2020")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

### Nix rules ##################################################################

RULES_NIXPKGS_TAG = "0.8.0"

RULES_NIXPKGS_SHA = "7aee35c95251c1751e765f7da09c3bb096d41e6d6dca3c72544781a5573be4aa"

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = RULES_NIXPKGS_SHA,
    strip_prefix = "rules_nixpkgs-%s" % RULES_NIXPKGS_TAG,
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v%s.tar.gz" % RULES_NIXPKGS_TAG],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")

rules_nixpkgs_dependencies()

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository", "nixpkgs_package", "nixpkgs_python_configure", "nixpkgs_sh_posix_configure")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:default.nix",
    nix_file_deps = [
        "//nix:sources.nix",
        "//nix:sources.json",
        "//nix:overlays.nix",
    ],
)

nixpkgs_sh_posix_configure(
    repository = "@nixpkgs",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "hspec-discover",
    attribute_path = "haskellPackages.hspec-discover",
    repository = "@nixpkgs",
)

# Haskell rules ################################################################

RULES_HASKELL_TAG = "0.13"

RULES_HASKELL_SHA = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6"

git_repository(
    name = "rules_haskell",
    commit = "b64079dbb212fef1f44866f43e8ae866059cf2bf",
    remote = "https://github.com/tweag/rules_haskell.git",
    shallow_since = "1635420837 +0200",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "ghc_8_10_4",
    repository = "@nixpkgs",
    version = "8.10.4",
)
