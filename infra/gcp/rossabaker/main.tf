provider "google" {
  project = "rossabaker-348417"
  region  = "us-central1"
  zone    = "us-central1-c"
}

resource "google_project_service" "compute" {
  project = "rossabaker-348417"
  service = "compute.googleapis.com"
}

resource "google_compute_instance" "vm_instance" {
  name         = "terraform-instance"
  machine_type = "e2-micro"

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-9"
    }
  }

  network_interface {
    # A default network is created for all GCP projects
    network = "default"
    access_config {
    }
  }
}

resource "google_compute_network" "vpc_network" {
  name                    = "terraform-network"
  auto_create_subnetworks = "true"
}
