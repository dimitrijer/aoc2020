self: super: {
  # Make ghc8107 the default package set for Haskell.
  haskellPackages = super.haskell.packages.ghc8107.override {
    overrides = self: super: { };
  };
  # Override ghc, adding all project dependencies as toolchain packages.
  ghc_8_10_7 = self.haskellPackages.ghcWithPackages (
    ps: with ps; [
      parsec
      containers
      mtl
      random
      array
      split
      hspec
    ]
  );
}
