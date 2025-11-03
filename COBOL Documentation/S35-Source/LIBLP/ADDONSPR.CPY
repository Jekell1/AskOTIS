      * COPYMEMBER: LIBLP/ADDONSPR
      *********************************************************************
      *               (LIBLP/ADDONSPR)
      *    FIND SPR ADDON INSURANCE WAS WRITTEN UNDER
      *
      ****** IMPORTANT NOTE ***************************************
      *
      *     SP1-FILE MUST BE OPEN BEFORE PERFORMING THIS ROUTINE
      *
      ****** IMPORTANT NOTE ***************************************
      *
      *   NAME: ADDONSPR
      *
      *   DESC: FIND-ADDON-SPR SECTION:
      *             THIS WILL FIND THE CORRECT SPR RECORD TO BE
      *             USED FOR REFUND OF ADDON INSURACE
      *
      *         FIND-ADDON-SPR-RESTORE SECTION:
      *             WILL RESTORE LOANS ORIGINAL SPR RECORD
      *
      *   IN  : MOVE THE INDEX OF INSURANCE TO ADDONSPR-SUB
      *          (WAS USING REB-SUB2)
      *                       EX.
      *                           FOR A&H ADDONSPR-SUB = 2
      *                               O2  ADDONSPR-SUB = 5
      *   OUT :
      *   COPY: ADDONSPRW
      *
      * REV:
      * BLV 020325 REMOVED CLOSES ON SP-FILE, CAUSED CLOSE ERRORS OF 47
      *            ON SPFILE IN CUSTMT; SP WILL GET CLOSED BY
      *            DECLARATIVES BEFORE PROGRAMS EXIT
      * BAH 150914 ADDED REB-SP-CONTRFRMLA TO HOLD ORIGINAL SPR VALUE, ADDONS
      *            READ THEIR OWN SPR THEN READ THE ORIGINAL AND THE
      *            SP-CONTRFRMLA COULD HAVE BEEN CHANGED IN CRNO, AMACC PL#901
      * BAH 2018.07.02 CREATE ADDONSPRW COPY MEMBER FOR OWN WORKERS, REMOVE
      *                THE REB- FIELDS AND USE ADDONSPR-
      * BAH 2019.03.01 REMOVED THE OPEN-SP1-FILE, MAKE SURE EACH PROGRAM
      *                HAS SP1-FILE OPEN. TOO MANY OPEN AND CLOSES WAS
      *                CAUSING SLOW SPEEDS
      *********************************************************************
       FIND-ADDON-SPR SECTION.

           IF ( LN-ADDON-FG NOT = "Y" ) OR ( ADDONSPR-SUB < 1 OR > 8 )
              MOVE " " TO ADDONSPR-SP1-KEY
              GO TO FIND-ADDON-SPR-EXIT.

           MOVE SP1-KEY       TO ADDONSPR-SP1-KEY.
           MOVE " "           TO ADDONSPR-SP-FLAG.
      * SAVE ORIGINAL SP-CONTRFRMLA, CRNO COULD HAVE CHANGED IT AND WHEN
      * YOU RESTORE THE ORIGINAL SPR IN HERE, YOU LOSE THAT CHANGE! SHOULD
      * REALLY BE SAVING THE RECORD INSTEAD OF REREADING IT.
           MOVE SP-CONTRFRMLA TO ADDONSPR-SP-CONTRFRMLA.

           PERFORM UNTIL ADDONSPR-SP-FLAG = "Y"
              IF SP-EXPDATE = 0
                 MOVE "Y" TO ADDONSPR-SP-FLAG
              ELSE
                 IF SP-EXPDATE NOT > LN-INSEFF-DATE(ADDONSPR-SUB)
                    IF SP-LAWCODE < 99
                       ADD 1 TO SP-LAWCODE
                    ELSE
                       MOVE ADDONSPR-SP1-KEY TO SP1-KEY
                       GO TO FIND-ADDON-SPR-EXIT
                    END-IF
                    PERFORM START-SP1-FILE
                    PERFORM READ-SP1-FILE-NEXT
                    IF IO-FG = 9
                       MOVE "Y" TO ADDONSPR-SP-FLAG
                       MOVE " " TO SP1-KEY
                       PERFORM FIND-ADDON-SPR-RESTORE
                    END-IF
                 ELSE
                    MOVE "Y" TO ADDONSPR-SP-FLAG
                 END-IF
              END-IF
           END-PERFORM.

       FIND-ADDON-SPR-EXIT.
           EXIT.

      ********************************************************
      *    TEST FOR RESTORE OF LOANS SPR RECORD
      ********************************************************
       FIND-ADDON-SPR-RESTORE SECTION.
           IF LN-ADDON-FG = "Y"
              IF ADDONSPR-SP1-KEY NOT = " "
                 IF SP1-KEY NOT = ADDONSPR-SP1-KEY
                    MOVE ADDONSPR-SP1-KEY TO SP1-KEY
                    PERFORM READ-SP1-FILE
                    IF IO-FG = 9
                       GO TO IO-ERROR
                    ELSE
      * IF YOU FIND THE ORIGINAL SPR, MAKE SURE THE SAVED CONTRFRMLA
      * IS RESTORED, CRNO COULD HAVE CHANGED IT SO WE HAVE TO SAVE IT
                       MOVE ADDONSPR-SP-CONTRFRMLA TO SP-CONTRFRMLA.
