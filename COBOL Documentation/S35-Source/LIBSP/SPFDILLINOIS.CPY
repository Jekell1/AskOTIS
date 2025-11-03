      **************************************************
      *  ILLINOIS VERITECT EXTRACT RECORD LAYOUT
      *  PIPE DELIMITED WITH CR/LF
      **************************************************
       FD  ILLINOIS-FILE
           RECORD VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  ILLINOIS-REC.
           03  IL-BYTE             PIC X OCCURS 2000 TIMES.
