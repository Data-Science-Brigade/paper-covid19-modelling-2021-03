for file in *.pdf; do
    echo $file `echo $file | sed 's/\(.*\.\)pdf/\1eps/'`
    inkscape $file --export-eps=`echo $file | sed 's/\(.*\.\)pdf/\1eps/'`
done

