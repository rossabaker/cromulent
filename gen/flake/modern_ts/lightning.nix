{ lib
, buildPythonPackage
, fetchPypi
, Jinja2
, PyYAML
, PyYAML
, arrow
, backoff
, beautifulsoup4
, click
, croniter
, dateutils
, deepdiff
, fastapi
, fsspec
, fsspec[http]
, inquirer
, lightning-cloud
, lightning-utilities
, numpy
, packaging
, packaging
, psutil
, pydantic
, python-multipart
, pytorch-lightning
, requests
, rich
, starlette
, starsessions
, torch
, torchmetrics
, tqdm
, traitlets
, typing-extensions
, urllib3
, uvicorn
, websocket-client
, websockets
}:

buildPythonPackage rec {
  pname = "lightning";
  version = "2022.10.25";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-RxW0o9xveu3ZI3Mhl2aF4TBSY5D7ifQuJajPH6ytpl0=";
  };

  propagatedBuildInputs = [
    Jinja2
    PyYAML
    PyYAML
    arrow
    backoff
    beautifulsoup4
    click
    croniter
    dateutils
    deepdiff
    fastapi
    fsspec
    fsspec[http]
    inquirer
    lightning-cloud
    lightning-utilities
    numpy
    packaging
    packaging
    psutil
    pydantic
    python-multipart
    pytorch-lightning
    requests
    rich
    starlette
    starsessions
    torch
    torchmetrics
    tqdm
    traitlets
    typing-extensions
    urllib3
    uvicorn
    websocket-client
    websockets
  ];

  pythonImportsCheck = [ "lightning" ];

  meta = with lib; {
    description = "The Deep Learning framework to train, deploy, and ship AI products Lightning fast";
    homepage = "https://github.com/Lightning-AI/lightning";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
