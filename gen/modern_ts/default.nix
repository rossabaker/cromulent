{ inputs, lib, moduleWithSystem, ... }:

let
  pythonPackages = ps: with ps; [
    plotly
    xgboost
    scikit-learn
    pandas
    (import ./catboost.nix)
    lightgbm
    (import ./category-encoders.nix)
    (import ./pmdarima.nix)
    ipywidgets
    jupyterlab
    black
    isort
    xlrd
    lxml
    openpyxl
    # kaleido
    pyarrow
    numpy
    scipy
    tqdm
    matplotlib
    humanize
    # optuna
    (import ./pytorch-forecasting.nix)
    numba
    holidays
    seaborn
    statsmodels
    (import ./statsforecast.nix)
    (import ./u8darts.nix)
    pytorch
    torchvision
    # torchaudio-bin
    # lightning
    kaggle
    (import ./missingno.nix)
    (import ./window-ops.nix)
    (import ./pymannkendall.nix)
    (import ./tsfel.nix)
    rich
    (import ./pytorch_tabular.nix)
    omegaconf
    (import ./tensor-sensor.nix)
    # (import ./jupyterlab-code-formatter.nix)
    (import ./aquirdturtle-collapsible-headings.nix)
  ];
in {
  perSystem = { config, self', inputs', pkgs, system, ... }: {
    devShells.modernTs = pkgs.devshell.mkShell {
      name = "modern-ts";
      packages = [
	(pkgs.python3.withPackages pythonPackages)
      ];
    };
  };
}
