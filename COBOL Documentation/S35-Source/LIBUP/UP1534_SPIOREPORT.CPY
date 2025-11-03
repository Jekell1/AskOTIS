      * COPYMEMBER: LIBUP/UP1534_SPIOREPORT.CPY
      ******************************************************************
      *
      * IDENTIFICATION:  LIBUP/UP1534_SPIOREPORT.CPY
      *
      * DESCRIPTION   :  UP1534 REPORT FILE (IO)
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      *
      ******************************************************************
       OPEN-REPORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           OPEN OUTPUT REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE-OUTPUT.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       OPEN-REPORT-FILE.

           PERFORM OPEN-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           OPEN I-O REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE.

           PERFORM READ-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           MOVE UP1534-REPORT-KEY TO E-KEYX.
           READ REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           MOVE UP1534-REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-REPORT-FILE.

           PERFORM START-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           MOVE UP1534-REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < UP1534-REPORT-KEY.
           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       WRITE-REPORT-FILE.

           PERFORM WRITE-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           MOVE UP1534-REPORT-KEY TO E-KEYX.

           WRITE REPORT-REC.

           IF ( IO-LOCKED )
             GO TO WRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       REWRITE-REPORT-FILE.

           PERFORM REWRITE-IT.
           MOVE UP1534-REPORT-IFN TO E-FILE.
           MOVE UP1534-REPORT-KEY TO E-KEYX.
           REWRITE REPORT-REC.

           IF ( IO-LOCKED )
              GO TO REWRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-REPORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.

