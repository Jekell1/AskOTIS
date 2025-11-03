      * COPYMEMBER: LIBAP/APOVN1RN
       LOAD-OVN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OVN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OVN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OVN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO OVN-ALL-MACHINE
              MOVE OVN-ALL-PATH             TO OVN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OVN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OVN-ALL-BASE
              MOVE FILPATH-MACHINE         TO OVN-ALL-MACHINE
              MOVE OVN-ALL-PATH             TO OVN-PATH.
       LIBAP-APOVN1RN.
      *    PERFORM FALL-THRU-IT.
           MOVE OVN-PATH TO E-FILE.
           GO TO IO-ERROR.
       OPEN-OVN1-FILE.
           PERFORM LOAD-OVN1-FILE.
           PERFORM OPEN-IT.
           MOVE OVN-PATH TO E-FILE.
           OPEN INPUT OVN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OVN1-FILE.
           IF IO-FG = 7
              CLOSE OVN-FILE
              GO TO OPEN-OVN1-FILE.
           UNLOCK OVN-FILE.
       READ-OVN1-FILE.
           PERFORM READ-IT.
           MOVE OVN-PATH TO E-FILE.
           MOVE OVN1-KEY TO E-KEYX.
           READ OVN-FILE KEY IS OVN1-KEY.
           IF IO-FG = 8
              GO TO READ-OVN1-FILE.
           UNLOCK OVN-FILE.
       READ-OVN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OVN-PATH TO E-FILE.
           MOVE OVN1-KEY TO E-KEYX.
           READ OVN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OVN1-FILE-NEXT.
           UNLOCK OVN-FILE.
       START-OVN1-FILE.
           PERFORM START-IT.
           MOVE OVN-PATH TO E-FILE.
           MOVE OVN1-KEY TO E-KEYX.
           START OVN-FILE KEY NOT < OVN1-KEY.
           UNLOCK OVN-FILE.
       CLOSE-OVN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OVN-FILE.
