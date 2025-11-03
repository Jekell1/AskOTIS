      * COPYMEMBER: LIBLP/LPLCCL

      ******************************************************************
      *    LPLCCL    LOAN CLASS  FILE CLEAR ROUTINE
      *
      * REV:
      * MG  990909 MOVING "N" TO LC-AFP-DEALER.
      * CS  010315 MOVE ZEROS TO LC-POOLID
      * BAH 151204 ADDED LC-LTOUCH-DATE
      *  CS 190426 CLEAR FUTURE FIELDS
      * BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-LC-FILE SECTION.
           MOVE SPACES TO LC-REC.

           MOVE "N"    TO LC-AFP-CLASS.
           MOVE 0      TO LC-CODE LC-POOLID LC-LTOUCH-DATE.

