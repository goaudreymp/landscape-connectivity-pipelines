#!/bin/bash

PAIRFILE="input.txt"
OUTDIR="./output"
CONFIGDIR="./configs"
LOGDIR="./logs"

CORES_PER_RUN=10

export JULIA_NUM_THREADS=$CORES_PER_RUN

mkdir -p "$CONFIGDIR" "$OUTDIR" "$LOGDIR"

timestamp() {
    date '+%Y-%m-%d %H:%M:%S'
}

while IFS=$'\t' read -r RES_FILE SRC_FILE RADIUS BLOCK_SIZE || [ -n "$RES_FILE" ]; do

    # Skip blank lines
    [ -z "$RES_FILE" ] && continue

    # Skip comment lines
    [[ "$RES_FILE" == \#* ]] && continue

    # Check that required fields are present
    if [ -z "$SRC_FILE" ] || [ -z "$RADIUS" ]; then
        echo "$(timestamp) ERROR: Invalid input line:"
        echo "  RES=$RES_FILE SRC=$SRC_FILE RADIUS=$RADIUS BLOCK_SIZE=$BLOCK_SIZE"
        continue
    fi

    # Default block size
    if [ -z "$BLOCK_SIZE" ]; then
        BLOCK_SIZE=1
    fi

    # --------------------------------------------------------
    # Prepare input
    # --------------------------------------------------------

    RES="$RES_FILE"

    if [ "$SRC_FILE" = "NONE" ]; then
        SRC=""
        SOURCE_FROM_RESISTANCE="true"
        SRC_BASENAME="NONE"
    else
        SRC="$SRC_FILE"
        SOURCE_FROM_RESISTANCE="false"
        SRC_BASENAME=$(basename "$SRC" .tif)
    fi

    RES_BASENAME=$(basename "$RES" .tif)

    # --------------------------------------------------------
    # Combination name
    # --------------------------------------------------------

    COMBINATION="${RES_BASENAME}_${SRC_BASENAME}_${RADIUS}"

    CONFIG="$CONFIGDIR/config_${COMBINATION}.ini"
    LOGFILE="$LOGDIR/${COMBINATION}.log"
    PROJECT_NAME="$OUTDIR/${COMBINATION}"

    # --------------------------------------------------------
    # Start
    # --------------------------------------------------------

    echo ""
    echo "$(timestamp) =================================================="
    echo "$(timestamp) Starting Omniscape run"
    echo "$(timestamp) RES        = $RES_FILE"
    echo "$(timestamp) SRC        = $SRC_FILE"
    echo "$(timestamp) RADIUS     = $RADIUS"
    echo "$(timestamp) BLOCK_SIZE = $BLOCK_SIZE"
    echo "$(timestamp) CORES      = $CORES_PER_RUN"
    echo "$(timestamp) CONFIG     = $CONFIG"
    echo "$(timestamp) LOG        = $LOGFILE"
    echo "$(timestamp) =================================================="

    # --------------------------------------------------------
    # Create config
    # --------------------------------------------------------

    cat > "$CONFIG" <<EOL
[Required]
resistance_file = $RES
source_file = $SRC
radius = $RADIUS
block_size = $BLOCK_SIZE
project_name = $PROJECT_NAME

[General options]
source_from_resistance = $SOURCE_FROM_RESISTANCE
calc_normalized_current = true
calc_flow_potential = true
parallelize = true
parallel_batch_size = 10

[Output options]
write_raw_currmap = true
EOL

    echo "$(timestamp) Config written to $CONFIG"

    # --------------------------------------------------------
    # Run Omniscape
    # --------------------------------------------------------

    echo "$(timestamp) Running Omniscape..."

    julia -e "using Omniscape; run_omniscape(\"$CONFIG\")" \
        2>&1 | tee "$LOGFILE"

    STATUS=${PIPESTATUS[0]}

    # --------------------------------------------------------
    # Result
    # --------------------------------------------------------

    if [ $STATUS -eq 0 ]; then
        echo "$(timestamp) Finished Omniscape run for $RES_FILE + $SRC_FILE"
    else
        echo "$(timestamp) FAILED Omniscape run for $RES_FILE + $SRC_FILE (exit $STATUS)"
    fi

    echo "$(timestamp) =================================================="

done < "$PAIRFILE"

echo ""
echo "$(timestamp) All Omniscape jobs finished."