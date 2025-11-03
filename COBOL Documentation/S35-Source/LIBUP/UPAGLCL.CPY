      ******************************************************************
      *    ASCII GENERAL LEDGER MASTER CLEAR
      *=================================================================
      * REV:
      * JKC 2024-1024 CHANGED USE OF AGL-BRANCH & AGL-ACCOUNT TO NOW
      *          USE AGL-BR-ACCOUNT.
      ******************************************************************
       CLEAR-AGL-FILE SECTION.
           MOVE ALL X"09" TO AGL-REC.

           MOVE 0 TO AGL-BR-ACCOUNT.
           MOVE 0 TO AGL-CONSOL AGL-OPENBAL AGL-NUMBER1 AGL-NUMBER2
                     AGL-LYOPENBAL.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
               MOVE 0 TO AGL-CYBAL(SUB)
                         AGL-LYBAL(SUB)
                         AGL-CYBUD(SUB)
                         AGL-LYBUD(SUB)
                         AGL-NYBUD(SUB)
           END-PERFORM.

           MOVE SPACES TO AGL-FLAG AGL-DESCRIPTION AGL-TYPE
                          AGL-CLEAR AGL-FS-FLAG AGL-DLALLOC-GL
                          AGL-SPANISH-DESC.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                MOVE SPACES TO  AGL-REPORTS(SUB)
           END-PERFORM.

