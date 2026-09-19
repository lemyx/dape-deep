"""Small CPU/GPU workload for a dape-deep smoke test."""

from __future__ import annotations

import os


def run_workload() -> None:
    """Run a deterministic tensor operation and print the selected device."""
    import torch

    require_cuda = os.environ.get("REQUIRE_CUDA", "1") != "0"
    if require_cuda and not torch.cuda.is_available():
        raise RuntimeError("CUDA was required, but torch.cuda.is_available() is false")

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    left = torch.arange(16, dtype=torch.float32, device=device).reshape(4, 4)
    right = torch.eye(4, dtype=torch.float32, device=device)
    result = left @ right

    print(f"torch={torch.__version__}", flush=True)
    print(f"device={device}", flush=True)
    print(f"sum={result.sum().item()}", flush=True)


if __name__ == "__main__":
    run_workload()
