for (( ; ; ))
do
   cd ~/Sample/trunk
   echo semper | sudo -S java -jar ./out/artifacts/sample_jar/sample.jar -r $1 $2 $3 $4 $5 $6 -Xms3072M -Xmx3072M -Xss4m
	if [ $? -eq 0 ]
	  then
	echo "End of the analysis"
	    exit
	else
	echo "Restarting the analysis"
	  fi
done
