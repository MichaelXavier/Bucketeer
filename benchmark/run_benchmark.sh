#!/bin/bash -e

port=3000
base_url=127.0.0.1:$port
iterations=1000
test_bucket=/consumers/test/buckets/feature

function check_ab {
  which ab > /dev/null
}

function run_benchmark {
  concurrency=$1
  path=$2

  ab -c $concurrency -k -q -n $iterations $base_url$path | grep "Requests per second" | awk '{ print $4 }'
}

function start_server {
  ./../dist/build/bucketeer/bucketeer -p $port -n benchmark &>/dev/null &
  echo $!
}

function benchmark_pass {
  outfile=reports/$1
  test_name=$2
  path=$3

  truncate -s 0 $outfile

  echo -n $test_name
  for concurrency in {1..10}; do
    echo -n "."
    result=$(run_benchmark $concurrency $path)
    echo "$concurrency,$result" >> $outfile
  done
  echo
}

function clear_bucket {
  curl -X DELETE $base_url/consumers/michael &>/dev/null
}

function create_bucket {
  curl -X POST -d "capacity=1000&restore_rate=10000" $base_url$test_bucket &>/dev/null
}

function reset_bucket {
  clear_bucket
  create_bucket
}

#benchmarks
function empty_set_benchmark {
  echo "*** Running empty set benchmark (does not hit redis)"
  benchmark_pass empty_set.csv "empty set" /
}

function non_empty_set_benchmark {
  reset_bucket

  echo "*** Running non empty set benchmark (does not hit redis)"
  benchmark_pass non_empty_set.csv "non empty set" /
}

function get_bucket_benchmark {
  reset_bucket

  echo "*** Running get bucket benchmark (retrieves from redis)"
  benchmark_pass get_bucket_benchmark.csv "get bucket" $test_bucket
}

function get_bucket_miss_benchmark {
  reset_bucket

  echo "*** Running get bucket benchmark miss (retrieves from redis)"
  benchmark_pass get_bucket_miss_benchmark.csv "get bucket miss" /consumers/test/buckets/bogus

}

function tick_bucket_benchmark {
  reset_bucket

  echo "*** Running tick bucket benchmark (writes to redis)"
  benchmark_pass tick_bucket.csv "tick bucket" $test_bucket
}

check_ab

pid=$(start_server)
outdir=reports

echo "Running server with pid $pid"
mkdir -p $outdir

# empty set benchmarks
empty_set_benchmark
non_empty_set_benchmark
get_bucket_benchmark
get_bucket_miss_benchmark
tick_bucket_benchmark

kill $pid &> /dev/null
