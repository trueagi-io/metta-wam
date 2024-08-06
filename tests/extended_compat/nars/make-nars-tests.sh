# Check if --force is in the command line arguments
for arg in "$@"
do
    if [ "$arg" = "--force" ]; then
        FORCE_REBUILD=true
        break
    fi
done

# Now use $FORCE_REBUILD to decide whether to skip file existence checks
if [ "$FORCE_REBUILD" = true ] || [ ! -f LIB_NARS.metta ]; then
    cd src
    sh build.sh
    cd ..
    echo  "" > LIB_NARS.metta
    cat src/NARS.metta >> LIB_NARS.metta
    echo  "" >> LIB_NARS.metta
fi

if [ -f "minnars/minnars.metta" ]; then

    if [ "$FORCE_REBUILD" = true ] || [ ! -f LIB_MINNARS.metta ]; then
	cat src/utils.metta src/logic/LOGIC.metta minnars/minnars.metta > LIB_MINNARS.metta 
    fi

    if [ "$FORCE_REBUILD" = true ] || [ ! -f TEST_METTA_minnars.metta ]; then
	cat LIB_MINNARS.metta minnars/input.metta > TEST_METTA_minnars.metta
    fi

    if [ "$FORCE_REBUILD" = true ] || [ ! -f TEST_METTALOG_minnars.metta ]; then
	echo "!(import! &self LIB_MINNARS.metta)" > TEST_METTALOG_minnars.metta
	cat minnars/input.metta >> TEST_METTALOG_minnars.metta
    fi

    echo time metta TEST_METTA_minnars.metta
    echo time mettalog --test TEST_METTA_minnars.metta
    echo time mettalog --test TEST_METTALOG_minnars.metta

    if [ -f "nalifier/nalifier.metta" ]; then

	if [ "$FORCE_REBUILD" = true ] || [ ! -f TEST_METTA_nalifier.metta ]; then
	    cat LIB_MINNARS.metta nalifier/nalifier.metta nalifier/input.metta > TEST_METTA_nalifier.metta
	fi
	if [ "$FORCE_REBUILD" = true ] || [ ! -f LIB_NALIFIER.metta ]; then
	    echo "!(import! LIB_MINNARS.metta)" > LIB_NALIFIER.metta
	    cat nalifier/nalifier.metta >> LIB_NALIFIER.metta
	fi
	if [ "$FORCE_REBUILD" = true ] || [ ! -f TEST_METTALOG_nalifier.metta ]; then
	    echo "!(import! &self LIB_NALIFIER.metta)" > TEST_METTALOG_nalifier.metta
	    cat nalifier/input.metta >> TEST_METTALOG_nalifier.metta
	fi

	echo time metta TEST_METTA_nalifier.metta
	echo time mettalog --test TEST_METTA_nalifier.metta
	echo time mettalog --test TEST_METTALOG_nalifier.metta
    fi

fi

MAKE_TEST () {

  if [ -f "tests/$1.metta" ]; then
      if [ "$FORCE_REBUILD" = true ] || [ ! -f "TEST_METTA_$1.metta" ]; then
	cat "src/NARS.metta" "tests/$1.metta" > "TEST_METTA_$1.metta"
      fi
      echo "time metta TEST_METTA_$1.metta"
      echo "time mettalog --test TEST_METTA_$1.metta"

      if [ "$FORCE_REBUILD" = true ] || [ ! -f "TEST_METTALOG_$1.metta" ]; then
	echo "" > "TEST_METTALOG_$1.metta"
	echo "!(import! &self LIB_MINNARS.metta)" >> "TEST_METTALOG_$1.metta"
	echo "" >> "TEST_METTALOG_$1.metta"
	cat "tests/$1.metta" >> "TEST_METTALOG_$1.metta"
      fi
      echo "time mettalog --test TEST_METTALOG_$1.metta"
  fi
}

MAKE_TEST tests0
MAKE_TEST tests1
MAKE_TEST tests2
MAKE_TEST tests3
MAKE_TEST tests4
MAKE_TEST tests5

mkdir -p $1/

mv LIB_* $1/
mv TEST_* $1/


if [ "$RUN_TESTS" = true ]; then

echo "Timing results for TEST_METTA_* files" > timed_results.txt

for test_file in TEST_METTA_*.metta; do
    if [ -f "$test_file" ]; then  # Check if file exists to avoid errors
        echo "Running $test_file ..."
        start_time=$(date +%s)  # Get current time in seconds
        time metta "$test_file"  # Run the metta command
        end_time=$(date +%s)  # Get current time in seconds after the command has finished
        duration=$((end_time - start_time))  # Calculate the duration
        echo "$test_file took $duration seconds" >> timed_results.txt  # Append the results to timed_results.txt
    fi
done

echo "All tests completed. Results stored in timed_results.txt."


echo "Timing results for TEST_METTALOG_* files" > timed_results.txt

for test_file in TEST_METTALOG_*.metta; do
    if [ -f "$test_file" ]; then  # Check if file exists to avoid errors
        echo "Running $test_file ..."
        start_time=$(date +%s.%N)  # Get current time in seconds with nanoseconds
        mettalog --test "$test_file"  # Run the mettalog command with test flag
        end_time=$(date +%s.%N)  # Get current time in seconds after the command has finished
        duration=$(echo "$end_time - $start_time" | bc)  # Calculate the duration using bc for float arithmetic
        echo "$test_file took $duration seconds" >> timed_results.txt  # Append the results to timed_results.txt
    fi
done

echo "All tests completed. Results stored in timed_results.txt."

fi
