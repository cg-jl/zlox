{
  description = "An empty project that uses Zig.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    zig.url = "github:mitchellh/zig-overlay";
    zls.url = "github:zigtools/zls";
    # Used for shell.nix
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    overlays = [
      # Other overlays
      (final: prev: {
        zigpkgs = inputs.zig.packages.${prev.system};
        zls = inputs.zls.packages.${prev.system}.zls;
      })
    ];

    # Our supported systems are the same supported systems as the Zig binaries
    systems = builtins.attrNames inputs.zig.packages;

  in
    flake-utils.lib.eachSystem systems (
      system: let
        pkgs = import nixpkgs {inherit overlays system; };


      in rec {
        devShells.default = pkgs.mkShell {
          name = "zlox";
          nativeBuildInputs = with pkgs; [
            zigpkgs.master
            valgrind
          ];
          buildInputs = with pkgs; [ zls ];
          shellHook = ''
            export VALGRIND_FLAGS="-I${pkgs.valgrind.dev}/include -L${pkgs.valgrind}/lib -O3"
          '';
        };

        # For compatibility with older versions of the `nix` binary
        devShell = self.devShells.${system}.default;
      }
    );
}
