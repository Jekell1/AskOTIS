      * COPYMEMBER: LIBMN/COMMON
      ******************************************************************
      *
      * IDENTIFICATION:  LIBMN/COMMON
      *
      * DESCRIPTION   :  COMMON ROUTINES FOR MAINTENANCE PROGRAMS
      *
      *=================================================================
      * REV:
      * KEC 2003.0401 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
      ******************************************************************
      *
      *    C O M M O N - M I S C - P A R A G R A P H S
      *
      ******************************************************************
       COMMON-MISC-PARAGRAPHS SECTION.
       GET-LOCK-KEY.
           MOVE EXT-CONAME-DAILY-PASSWORD        TO LOCK-KEY.
       MESSAGE-ACTIVE-RECORD.
           MOVE TEXT-ACTIVE-RECORD TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-ALREADY-CREATED.
           MOVE TEXT-ALREADY-CREATED TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-CHANGE-AGAIN.
           MOVE TEXT-CHANGE-AGAIN TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-CREATE-ONLY-ON-P1.
           MOVE TEXT-CREATE-ONLY-ON-PAGE-01 TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-RECORD-CHANGED.
           MOVE TEXT-RECORD-CHANGED TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-RECORD-DELETED.
           MOVE TEXT-RECORD-DELETED TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-RECORD-NOT-FOUND.
           MOVE TEXT-RECORD-NOT-FOUND TO MESS.
           PERFORM SEND-MESS.
       MESSAGE-UNDERGONE-CHANGE.
           MOVE TEXT-UNDERGONE-CHANGE TO MESS.
           PERFORM SEND-MESS.
