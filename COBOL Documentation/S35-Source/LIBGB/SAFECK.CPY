      * COPYMEMBER: LIBGB/SAFECK
      * 050810 BAH ADDED COPY OF SPANCK FOR WORLD SPANISH/PESO TRANSLATION
      * AMM 2014.0822 PR#4570 MULLEN/KSTREET; PER JUDY'S EMAIL THEY NEED
      *       TO ACCOMMODATE A LONGER ALP LINE FOR MILLION DOLLAR CHECKS
      *       ADDED ALP-LINEX  TO SAFECKW  PIC X(95)
      ********************************************
      *      S A F E C K    R O U T I N E        *
      ********************************************
       SAFECK SECTION.
       ALP-1.
           MOVE 0 TO NEXT-3.
           MOVE "***" TO ALP-LINE.
           MOVE "***" TO ALP-LINEX.
           MOVE ND-CENTS TO D-CENTS.
           MOVE "NO DOLLARS" TO ALP-DOLLARS.

           IF ND-M > 0
              MOVE ALP-1-DOLS(ND-M) TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS
              MOVE "MILLION" TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS.
       ALP-2.
           IF ND-B > 0
              MOVE ALP-1-DOLS(ND-B) TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS
              MOVE "HUNDRED" TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS.
           IF ND-E > 0
              IF ND-E < 20
                 MOVE ALP-1-DOLS(ND-E) TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS
              ELSE
                 COMPUTE ND-SUB = ND-E / 10 - 1
                 MOVE ALP-2-DOLS (ND-SUB) TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS
                 IF ND-D > 0
                    MOVE ALP-1-DOLS(ND-D) TO ALP-DOLLARS
                    PERFORM MOVE-ALP-DOLS.
           IF ND-A > 0
              IF NEXT-3 = 0
                 MOVE "THOUSAND" TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS.
           IF NEXT-3 = 0
              MOVE 1 TO NEXT-3
              MOVE ND-F TO ND-A
              GO TO ALP-2.
           IF ALP-DOLLARS = "NO DOLLARS"
              PERFORM MOVE-ALP-DOLS.
           MOVE CENTS-MSG TO ALP-DOLLARS.
           PERFORM MOVE-ALP-DOLS.
           GO TO EXIT-SAFECK.
       MOVE-ALP-DOLS.
           INSPECT ALP-LINE REPLACING FIRST "  " BY " +".
           INSPECT ALP-LINE
              REPLACING FIRST "+         " BY ALP-DOLLARS.

           INSPECT ALP-LINEX REPLACING FIRST "  " BY " +".
           INSPECT ALP-LINEX
              REPLACING FIRST "+         " BY ALP-DOLLARS.

       EXIT-SAFECK.
           EXIT.
