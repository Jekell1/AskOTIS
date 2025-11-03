      * COPYMEMBER: LIBGB/ARRAYBRW
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR BRANCH PROCESSING (BRFILE)
      *
      *              USED TO LOAD *ALL* BRFILE
      *              INTO AN INTERNAL ARRAY FOR IMPROVING PERFORMANCE.
      *
      * NEEDED     : LIBGB/GB01BR.CPY   (NEEDS 78 BR-SIZE)
      *
      *=================================================================
      * REV:
      * JKC 2023-1205 ***NEW***
      * MB  2024-0205 ADDED ARRAYBR-LOAD-ALL-SW (USED TO TELL ARRAYBR IF
      *                   IT SHOULD LOADED ALL BRANCHES INTO ARRAY IN 
      *                   INITIAL LOAD PROCESSES.  IF "N" BRANCHES ARE LOADED
      *                   INTO THE ARRAY AS THEY ARE REQUESTED IN ARRAYBR-READ
      *               ADDED ARRAYBR-BRANCHES-LOADED TO KEEP TRACK OF HOW 
      *               MANY BRANCHES HAVE BEEN LOADED INTO THE ARRAY
      ******************************************************************
       77  ARRAYBR-ELE                 PIC 9(4)      VALUE ZEROES.
       77  ARRAYBR-MAX                 PIC 9(4)      VALUE ZEROES.
       77  ARRAYBR-BRANCHES-LOADED     PIC 9(4)      VALUE ZEROES.
       77  ARRAYBR-LOAD-ALL-SW         PIC X         VALUE "Y".

       01  ARRAYBR-BR-KEY.
           03  ARRAYBR-BR-NO           PIC 9(4).

       01  ARRAYBR-FILE.
           03  ARRAYBR-REC             PIC X(BR-SIZE)
                                           OCCURS 1 TO 5000
                                           DEPENDING ON ARRAYBR-MAX.

       01  ARRAYBR-REC-READ            PIC X(BR-SIZE).

