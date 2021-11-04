self: super: {
  # Make ghc8104 the default package set for Haskell.
  haskellPackages = super.haskell.packages.ghc8104.override {
    overrides = self: super: { };
  };
  # Override ghc, adding all project dependencies as toolchain packages.
  ghc_8_10_4 = self.haskellPackages.ghcWithPackages (
    ps: with ps; [
      parsec
      containers
      mtl
      random
      array
      split
      hspec
      ormolu
    ]
  );
}
