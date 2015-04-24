
Fuente de datos: https:d396qusza40orc.cloudfront.netgetdata2Fprojectfiles2FUCI20HAR20Dataset.zip

Descripción original: http:archive.ics.uci.edumldatasetsHumanActivityRecognitionUsingSmartphones

El  scriptde R adjunto (run_analysis.R) realiza las siguientes acciones para limpiar los datos:

Combina los conjuntos training y test para crear un conjunto de datos, cuyo nombre es train/X_train.txt, test/X_test.txt, train/subject_train.txt, test/subject_test.txt, train/y_train.txt y test/y_test.txt.

Se pasa a leer features.txt extrayendo únicamente la media y la desviación típica de cada medida.

Tras leer activity_labels.txt se a`lican nombres de actividad descriptivos a los nombres de actividad en el conjunto de datos:

walking

walkingupstairs

walkingdownstairs

sitting

standing

laying

También se utilizan etiquetas con nombres descriptivoss apropiados para el conjunto de datos, se quitan guiones, paréntesis, .... El resultado se archiva en fusión_datos_limpios.txt. Los nombres de los atributos son similares a los siguientes:

tbodyacc-mean-x 

tbodyacc-mean-y 

tbodyacc-mean-z 

tbodyacc-std-x 

tbodyacc-std-y 

tbodyacc-std-z 

tgravityacc-mean-x 

tgravityacc-mean-y

Para terminar, el script crea un segundo eindependiente conjunto de datos ordenado con la media de cada medida para cada actividad y cada materia.
