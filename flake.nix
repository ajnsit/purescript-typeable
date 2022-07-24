{
  description = "Test monadic PureScript programs using state machine based models";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    easy-purescript-src = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { utils, nixpkgs-stable, nixpkgs-unstable, easy-purescript-src, ... }:
    let name = "purescript-typeable";
    in
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        easy-purescript = import easy-purescript-src { pkgs = pkgs.stable; };
        pkgs = {
          stable = import nixpkgs-stable {
            inherit system;
            overlays = [ (_: _: { inherit (easy-purescript) purs-tidy pulp spago; purescript = easy-purescript.purs-0_15_2; }) ];
          };
          unstable = import nixpkgs-unstable { inherit system; };
        };
      in
      {
        devShell = import ./shell.nix { inherit name pkgs; };
      });
}
