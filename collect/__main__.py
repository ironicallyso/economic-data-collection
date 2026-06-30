from __future__ import annotations

import argparse

from dotenv import load_dotenv

from collect import bea, bls, fred
from collect.config import load_config


def main() -> None:
    load_dotenv()

    parser = argparse.ArgumentParser(
        prog="collect", description="Collect BLS/BEA/FRED economic data."
    )
    parser.add_argument("--source", choices=["bls", "bea", "fred"], required=True)
    parser.add_argument("--mode", choices=["historical", "latest"], required=True)
    parser.add_argument("--config", default="config.yaml", help="Path to config.yaml")
    args = parser.parse_args()

    config = load_config(args.config)

    dispatch = {
        ("bls", "historical"): bls.run_historical,
        ("bls", "latest"): bls.run_latest,
        ("bea", "historical"): bea.run_historical,
        ("bea", "latest"): bea.run_latest,
        ("fred", "historical"): fred.run_historical,
        ("fred", "latest"): fred.run_latest,
    }
    dispatch[(args.source, args.mode)](config)


if __name__ == "__main__":
    main()
