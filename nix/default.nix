{ sources ? import ./sources.nix, config ? { }, overlays ? [ ] }:

import sources.nixpkgs {
  overlays = [ (import ./overlays.nix) ];
  config = { };
}
