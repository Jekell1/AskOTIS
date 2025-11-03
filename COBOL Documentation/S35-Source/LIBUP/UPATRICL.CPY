      * COPYMEMBER: LIBUP/UPATRICL
      ******************************************************************
      *
      * DESCRIPTION   :  ASCII TRANSACTION FILE CLEAR ROUTINE - TYPE 'I'
      *                    INSURANCE CLAIMS
      *
      * BAH 20210426 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-ATRI-FILE SECTION.
           MOVE ALL X"09" TO ATRI-REC.

           MOVE 0 TO ATI-BRNO ATI-KEY-DATE ATI-SEQ
                     ATI-TYPE ATI-AMOUNT ATI-DATE ATI-DRAFT
                     ATI-PRODUCER ATI-POSTTIME ATI-POSTDATE ATI-INSCOMP
                     ATI-LTOUCH-DATE ATI-DAYS-PAID ATI-FROM-DATE
                     ATI-TO-DATE.

           MOVE SPACES TO ATI-WAIS ATI-CLAIM-NUMBER
                          ATI-LNAME ATI-FNAME ATI-USERID ATI-PMT-TYPE 
                          ATI-FILLER ATI-FILLER2 ATI-FILLER3.

       CLEAR-ATRI-FILE-EXIT.
           EXIT.
