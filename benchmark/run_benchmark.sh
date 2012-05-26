#!/bin/bash -e

port=3000
base_url=127.0.0.1:$port
iterations=1000
test_bucket=/consumers/test/buckets/feature
gnuplot_config="set xlabel 'concurrency'; set ylabel 'Req/Sec'; set datafile separator ','; set style data histograms; set style fill solid 1.0 border -1; set yrange [0:]; plot '-' with boxes"
max_concurrency=$1
echo "running with max concurrency $max_concurrency"

function check_ab {
  which ab > /dev/null
}

function check_gnuplot {
  which gnuplot > /dev/null
}

function check_redis {
  redis-cli ping > /dev/null
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
  basename=$1
  outfile=reports/$basename.csv
  test_name=$2
  path=$3

  echo "set title '$test_name'; set terminal png; set output 'reports/$basename.png'; $gnuplot_config" > $outfile

  echo -n $test_name
  for ((concurrency=1; concurrency < $max_concurrency; concurrency++)); do
    echo -n "."
    result=$(run_benchmark $concurrency $path)
    echo "$concurrency,$result" >> $outfile
  done
  echo
  plot $basename
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
  benchmark_pass empty_set "empty set" /

}

function plot {
  echo "plotting $1.csv"
  gnuplot < reports/$1.csv
}


function non_empty_set_benchmark {
  reset_bucket

  echo "*** Running non empty set benchmark (does not hit redis)"
  benchmark_pass non_empty_set "non empty set" /
}

function get_bucket_benchmark {
  reset_bucket

  echo "*** Running get bucket benchmark (retrieves from redis)"
  benchmark_pass get_bucket_benchmark "get bucket" $test_bucket
}

function get_bucket_miss_benchmark {
  reset_bucket

  echo "*** Running get bucket benchmark miss (retrieves from redis)"
  benchmark_pass get_bucket_miss_benchmark "get bucket miss" /consumers/test/buckets/bogus

}

function tick_bucket_benchmark {
  reset_bucket

  echo "*** Running tick bucket benchmark (writes to redis)"
  benchmark_pass tick_bucket "tick bucket" $test_bucket
}

check_ab
check_gnuplot
check_redis

pid=$(start_server)
outdir=reports

trap "kill $pid &> /dev/null" EXIT

echo "Running server with pid $pid"
rm -rf $outdir
mkdir -p $outdir

# empty set benchmarks
empty_set_benchmark
non_empty_set_benchmark
get_bucket_benchmark
get_bucket_miss_benchmark
tick_bucket_benchmark
