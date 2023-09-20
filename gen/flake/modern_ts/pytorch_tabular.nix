{ lib
, buildPythonPackage
, fetchPypi
, PyYAML
, category-encoders
, einops
, ipywidgets
, matplotlib
, numpy
, omegaconf
, pandas
, protobuf
, pytorch-lightning
, pytorch-tabnet
, rich
, scikit-learn
, tensorboard
, torch
, torchmetrics
}:

buildPythonPackage rec {
  pname = "pytorch_tabular";
  version = "1.0.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-E/SDFqpvam32FdaIYL9vDJxuTvrG3U/LIGWaAs5AnCA=";
  };

  propagatedBuildInputs = [
    PyYAML
    category-encoders
    einops
    ipywidgets
    matplotlib
    numpy
    omegaconf
    pandas
    protobuf
    pytorch-lightning
    pytorch-tabnet
    rich
    scikit-learn
    tensorboard
    torch
    torchmetrics
  ];

  pythonImportsCheck = [ "pytorch_tabular" ];

  meta = with lib; {
    description = "A standard framework for using Deep Learning for tabular data";
    homepage = "https://github.com/manujosephv/pytorch_tabular";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
