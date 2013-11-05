for (( ; ; ))
do
   cd ~/Sample
   java -jar ./out/artifacts/sample_jar/sample.jar -r $1 $2 $3 $4 $5 $6 -Xms2072M -Xmx2072M -Xss4m
	if [ $? -eq 0 ]
	  then
	echo "End of the analysis"
	    exit
	else
	echo "Restarting the analysis"
	  fi
done
