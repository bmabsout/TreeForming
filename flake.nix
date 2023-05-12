{
  description = "An environment for testing program contruction ideas";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  inputs.nix-vscode-extensions.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, flake-utils, nixpkgs, nix-vscode-extensions }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          nix-extensions = nix-vscode-extensions.extensions.${system};
          extensions = (with pkgs.vscode-extensions; [
            bbenoist.nix
            haskell.haskell
            justusadam.language-haskell
            jock.svg
            nix-extensions.vscode-marketplace.ms-vscode.sublime-keybindings
          ]);
          
          vscodium-with-extensions = pkgs.vscode-with-extensions.override {
            vscode = pkgs.vscodium;
            vscodeExtensions = extensions;
          };
          ghc-with-deps = pkgs.ghc.withPackages (ps: with ps; [
            diagrams
            transformers
            recursion-schemes
            deriving-compat
            polysemy
            polysemy-plugin
            transformers-compat
            polysemy-zoo
          ]);
      in {

         devShell = pkgs.mkShell {
           buildInputs = [
             pkgs.inotify-tools
             ghc-with-deps
             pkgs.feh
             pkgs.haskell-language-server
             vscodium-with-extensions
           ];
         };
      }


    );
}
