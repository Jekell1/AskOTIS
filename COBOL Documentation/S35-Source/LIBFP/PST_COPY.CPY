      * COPYMEMBER: LIBFP/PST.COPY
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.COPY
      *
      * DESCRIPTION   :  LASER (PST) FORM COPY LIBRARY
      *                                              PARAGRAPHS/SECTIONS
      *                  - WITH ***NO*** PAYOFF/REFUNDS
      *
      *=================================================================
      * REV:
      * KEC 2011.0930 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW*** POSTSCRIPT
      * BAH 2018.0918 CHANGED CRNORC TO CRNOR2
      *
      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE LONPM-HELP--FILE           TO HELP-LINK-FILE.
           MOVE LONPM-HELP--DIR            TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              COPY "LIBLP/CHCKI_EVA.CPY".
              COPY "LIBLP/BCHKI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - C O P Y - L I B R A R Y - P A R A G R A P H S
      *
      ******************************************************************
       PST-COPY-MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       PST-COPY-MISC-CRNORC.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD              TO CRNOR2-SYSDATE.
           MOVE LN-ACCTNO                  TO CRNOR2-ACCTNO.
           MOVE "CL/CRNOR2"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

       PST-COPY-MISC-PARA-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SAFECK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBSP/WORDS.CPY".
