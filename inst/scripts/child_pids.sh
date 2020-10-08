# given a PID, output all its child PIDs recursively
list_children() {
  for j in $(pgrep -P $1); do
    echo $j
    echo $(list_children $j)
  done
}

for i in $@; do
  list_children $i
done
