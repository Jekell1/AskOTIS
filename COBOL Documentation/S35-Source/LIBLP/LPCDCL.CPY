      * COPYMEMBER: LIBLP/LPCDCL
      ******************************************************************
      * CLEAR CD FILE
      *
      * REV:
      * BAH 2019-0128 NEW, FOR COMBINED LAYOUT
      *  CS 2019-0513 CLEAR FUTURE FIELDS
      * JKC 2021-1118 REMOVED CD-ELUNITPER-CD 
      ******************************************************************
       CLEAR-CD-FILE SECTION.
           MOVE SPACES TO CD-REC.

           MOVE 0 TO CD-LTOUCH-DATE  
                     CD-BR-GL-BRANCH    CD-BR-GL-ACCT
                     CD-BR-OT-GL-BRANCH CD-BR-OT-GL-ACCT

                     CD-ELGRACE  CD-ELRATE   CD-ELMINCHG
                     CD-ELMAXAMT CD-ELDELFAC CD-ELYRTYPE

                     CD-PO-TITLE-HOLD-DAYS

                     CD-RE-RESCHEDULE-TIME

                     CD-FC-FEEAMT.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              MOVE 0 TO CD-CT-INSTAXAMT(SUB)
              MOVE 0 TO CD-CY-INSTAXAMT(SUB)
           END-PERFORM.
