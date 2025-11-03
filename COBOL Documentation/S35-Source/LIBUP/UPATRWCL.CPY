      * COPYMEMBER: LIBUP/UPATRWCL

      ***********************************************************************
       CLEAR-ATRW-FILE SECTION.
      ***********************************************************************
           MOVE ALL X"09" TO ATRW-REC.

           MOVE 0 TO ATW-BRNO ATW-DATE ATW-TILLNO.
           MOVE 0 TO ATW-LTOUCH-DATE.

           PERFORM VARYING TRWSUB FROM 1 BY 1 UNTIL TRWSUB > 20
              MOVE 0 TO ATW-AMOUNT(TRWSUB) ATW-OVERSHORT(TRWSUB)
                        ATW-TIME(TRWSUB)
              MOVE SPACES TO ATW-POSTNAME(TRWSUB)
           END-PERFORM.
