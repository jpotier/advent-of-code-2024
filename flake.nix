{
  description = "A Hello World for AoC";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        aoc24 = final.haskellPackages.callCabal2nix "aoc24" ./. {};
      });
      packages = forAllSystems (system: {
         aoc24 = nixpkgsFor.${system}.aoc24;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.aoc24);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.aoc24];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            fourmolu
            eventlog2html
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
          hpack
        '';
        });
  };
}
