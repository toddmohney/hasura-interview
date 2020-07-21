#!/bin/bash

set -euo pipefail

cd /usr/local/src/workspace/migrations

sqitch deploy --target $DATABASE_URL

exec "$@"
