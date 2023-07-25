{ lib
, buildPythonPackage
, fetchPypi
, fastapi
, lightning
, matplotlib
, optuna
, pandas
, pytorch-optimizer
, scikit-learn
, scipy
, statsmodels
, torch
}:

buildPythonPackage rec {
  pname = "pytorch-forecasting";
  version = "1.0.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-X36PXAWJwUWA+QpqLbBu0qeaIlBm5WgBXfJFPSrd0g4=";
  };

  propagatedBuildInputs = [
    fastapi
    lightning
    matplotlib
    optuna
    pandas
    pytorch-optimizer
    scikit-learn
    scipy
    statsmodels
    torch
  ];

  pythonImportsCheck = [ "pytorch.forecasting" ];

  meta = with lib; {
    description = "Forecasting timeseries with PyTorch - dataloaders, normalizers, metrics and models";
    homepage = "https://pytorch-forecasting.readthedocs.io";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
