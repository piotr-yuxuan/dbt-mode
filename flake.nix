# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
{
  description = "Development shell of dbt-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/31ecbaca1bdefa45dffc409f0250bd8039ea0010";
  };

  outputs = { nixpkgs, ... }:
    let
      shell = pkgs: pkgs.mkShell {
        packages = [
          (pkgs.emacs.pkgs.withPackages (epkgs: [
            # Optional for users: dbt-mode uses them only when they are
            # loaded. Here so that the tests of that use can run.
            epkgs.consult
            epkgs.embark
            epkgs.lsp-mode
            epkgs.package-lint
            epkgs.relint
          ]))
          pkgs.git
          # xref-matches-in-files searches with grep or ripgrep.
          pkgs.ripgrep
          # tests/bin/dbt runs the scripted dbt lsp, fake_dbt_lsp.py.
          pkgs.python3
        ];
      };
    in
    {
      devShells.x86_64-linux.default = shell nixpkgs.legacyPackages.x86_64-linux;
      devShells.aarch64-darwin.default = shell nixpkgs.legacyPackages.aarch64-darwin;
    };
}
