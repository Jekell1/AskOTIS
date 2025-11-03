       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOFDBI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOFDBI
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : FDBFILE 
      * TABLE DESC    : FORM DATABASE FILE
      * OUTPUT FILE   : XPC/IOFDBI.PRE
      * USED FOR      : LIBLP/LPWSFDB.CPY  --- QFDB-REC
      *                 LIBLP/LPFDB1IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOFDBI.CBL S34 05/18/21-08:02:15 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LPWSFDB.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBLP/LPWSFDB.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QFDB1-WHERE-BEG-KEY.
           03  QFDB1-WBEG-DBNO            PIC 9(02).

       01  QFDB1-WHERE-END-KEY.
           03  QFDB1-WEND-DBNO            PIC 9(02).

       01  QFDB-REC.
           03  QFDB-DBNO                  PIC 9(02).
           03  QFDB-LTOUCH-DATE           PIC X(10).
           03  QFDB-DNAME-1               PIC X(08).
           03  QFDB-DOFFSET-1             PIC 9(04).
           03  QFDB-DDIGITS-1             PIC 9(02).
           03  QFDB-DTYPE-1               PIC 9(02).
           03  QFDB-DDECIMAL-1            PIC 9(02).
           03  QFDB-DNAME-2               PIC X(08).
           03  QFDB-DOFFSET-2             PIC 9(04).
           03  QFDB-DDIGITS-2             PIC 9(02).
           03  QFDB-DTYPE-2               PIC 9(02).
           03  QFDB-DDECIMAL-2            PIC 9(02).
           03  QFDB-DNAME-3               PIC X(08).
           03  QFDB-DOFFSET-3             PIC 9(04).
           03  QFDB-DDIGITS-3             PIC 9(02).
           03  QFDB-DTYPE-3               PIC 9(02).
           03  QFDB-DDECIMAL-3            PIC 9(02).
           03  QFDB-DNAME-4               PIC X(08).
           03  QFDB-DOFFSET-4             PIC 9(04).
           03  QFDB-DDIGITS-4             PIC 9(02).
           03  QFDB-DTYPE-4               PIC 9(02).
           03  QFDB-DDECIMAL-4            PIC 9(02).
           03  QFDB-DNAME-5               PIC X(08).
           03  QFDB-DOFFSET-5             PIC 9(04).
           03  QFDB-DDIGITS-5             PIC 9(02).
           03  QFDB-DTYPE-5               PIC 9(02).
           03  QFDB-DDECIMAL-5            PIC 9(02).
           03  QFDB-DNAME-6               PIC X(08).
           03  QFDB-DOFFSET-6             PIC 9(04).
           03  QFDB-DDIGITS-6             PIC 9(02).
           03  QFDB-DTYPE-6               PIC 9(02).
           03  QFDB-DDECIMAL-6            PIC 9(02).
           03  QFDB-DNAME-7               PIC X(08).
           03  QFDB-DOFFSET-7             PIC 9(04).
           03  QFDB-DDIGITS-7             PIC 9(02).
           03  QFDB-DTYPE-7               PIC 9(02).
           03  QFDB-DDECIMAL-7            PIC 9(02).
           03  QFDB-DNAME-8               PIC X(08).
           03  QFDB-DOFFSET-8             PIC 9(04).
           03  QFDB-DDIGITS-8             PIC 9(02).
           03  QFDB-DTYPE-8               PIC 9(02).
           03  QFDB-DDECIMAL-8            PIC 9(02).
           03  QFDB-DNAME-9               PIC X(08).
           03  QFDB-DOFFSET-9             PIC 9(04).
           03  QFDB-DDIGITS-9             PIC 9(02).
           03  QFDB-DTYPE-9               PIC 9(02).
           03  QFDB-DDECIMAL-9            PIC 9(02).
           03  QFDB-DNAME-10              PIC X(08).
           03  QFDB-DOFFSET-10            PIC 9(04).
           03  QFDB-DDIGITS-10            PIC 9(02).
           03  QFDB-DTYPE-10              PIC 9(02).
           03  QFDB-DDECIMAL-10           PIC 9(02).
           03  QFDB-DNAME-11              PIC X(08).
           03  QFDB-DOFFSET-11            PIC 9(04).
           03  QFDB-DDIGITS-11            PIC 9(02).
           03  QFDB-DTYPE-11              PIC 9(02).
           03  QFDB-DDECIMAL-11           PIC 9(02).
           03  QFDB-DNAME-12              PIC X(08).
           03  QFDB-DOFFSET-12            PIC 9(04).
           03  QFDB-DDIGITS-12            PIC 9(02).
           03  QFDB-DTYPE-12              PIC 9(02).
           03  QFDB-DDECIMAL-12           PIC 9(02).
           03  QFDB-DNAME-13              PIC X(08).
           03  QFDB-DOFFSET-13            PIC 9(04).
           03  QFDB-DDIGITS-13            PIC 9(02).
           03  QFDB-DTYPE-13              PIC 9(02).
           03  QFDB-DDECIMAL-13           PIC 9(02).
           03  QFDB-DNAME-14              PIC X(08).
           03  QFDB-DOFFSET-14            PIC 9(04).
           03  QFDB-DDIGITS-14            PIC 9(02).
           03  QFDB-DTYPE-14              PIC 9(02).
           03  QFDB-DDECIMAL-14           PIC 9(02).
           03  QFDB-DNAME-15              PIC X(08).
           03  QFDB-DOFFSET-15            PIC 9(04).
           03  QFDB-DDIGITS-15            PIC 9(02).
           03  QFDB-DTYPE-15              PIC 9(02).
           03  QFDB-DDECIMAL-15           PIC 9(02).
           03  QFDB-DNAME-16              PIC X(08).
           03  QFDB-DOFFSET-16            PIC 9(04).
           03  QFDB-DDIGITS-16            PIC 9(02).
           03  QFDB-DTYPE-16              PIC 9(02).
           03  QFDB-DDECIMAL-16           PIC 9(02).
           03  QFDB-DNAME-17              PIC X(08).
           03  QFDB-DOFFSET-17            PIC 9(04).
           03  QFDB-DDIGITS-17            PIC 9(02).
           03  QFDB-DTYPE-17              PIC 9(02).
           03  QFDB-DDECIMAL-17           PIC 9(02).
           03  QFDB-DNAME-18              PIC X(08).
           03  QFDB-DOFFSET-18            PIC 9(04).
           03  QFDB-DDIGITS-18            PIC 9(02).
           03  QFDB-DTYPE-18              PIC 9(02).
           03  QFDB-DDECIMAL-18           PIC 9(02).
           03  QFDB-DNAME-19              PIC X(08).
           03  QFDB-DOFFSET-19            PIC 9(04).
           03  QFDB-DDIGITS-19            PIC 9(02).
           03  QFDB-DTYPE-19              PIC 9(02).
           03  QFDB-DDECIMAL-19           PIC 9(02).
           03  QFDB-DNAME-20              PIC X(08).
           03  QFDB-DOFFSET-20            PIC 9(04).
           03  QFDB-DDIGITS-20            PIC 9(02).
           03  QFDB-DTYPE-20              PIC 9(02).
           03  QFDB-DDECIMAL-20           PIC 9(02).
           03  QFDB-DNAME-21              PIC X(08).
           03  QFDB-DOFFSET-21            PIC 9(04).
           03  QFDB-DDIGITS-21            PIC 9(02).
           03  QFDB-DTYPE-21              PIC 9(02).
           03  QFDB-DDECIMAL-21           PIC 9(02).
           03  QFDB-DNAME-22              PIC X(08).
           03  QFDB-DOFFSET-22            PIC 9(04).
           03  QFDB-DDIGITS-22            PIC 9(02).
           03  QFDB-DTYPE-22              PIC 9(02).
           03  QFDB-DDECIMAL-22           PIC 9(02).
           03  QFDB-DNAME-23              PIC X(08).
           03  QFDB-DOFFSET-23            PIC 9(04).
           03  QFDB-DDIGITS-23            PIC 9(02).
           03  QFDB-DTYPE-23              PIC 9(02).
           03  QFDB-DDECIMAL-23           PIC 9(02).
           03  QFDB-DNAME-24              PIC X(08).
           03  QFDB-DOFFSET-24            PIC 9(04).
           03  QFDB-DDIGITS-24            PIC 9(02).
           03  QFDB-DTYPE-24              PIC 9(02).
           03  QFDB-DDECIMAL-24           PIC 9(02).
           03  QFDB-DNAME-25              PIC X(08).
           03  QFDB-DOFFSET-25            PIC 9(04).
           03  QFDB-DDIGITS-25            PIC 9(02).
           03  QFDB-DTYPE-25              PIC 9(02).
           03  QFDB-DDECIMAL-25           PIC 9(02).
           03  QFDB-DNAME-26              PIC X(08).
           03  QFDB-DOFFSET-26            PIC 9(04).
           03  QFDB-DDIGITS-26            PIC 9(02).
           03  QFDB-DTYPE-26              PIC 9(02).
           03  QFDB-DDECIMAL-26           PIC 9(02).
           03  QFDB-DNAME-27              PIC X(08).
           03  QFDB-DOFFSET-27            PIC 9(04).
           03  QFDB-DDIGITS-27            PIC 9(02).
           03  QFDB-DTYPE-27              PIC 9(02).
           03  QFDB-DDECIMAL-27           PIC 9(02).
           03  QFDB-DNAME-28              PIC X(08).
           03  QFDB-DOFFSET-28            PIC 9(04).
           03  QFDB-DDIGITS-28            PIC 9(02).
           03  QFDB-DTYPE-28              PIC 9(02).
           03  QFDB-DDECIMAL-28           PIC 9(02).
           03  QFDB-DNAME-29              PIC X(08).
           03  QFDB-DOFFSET-29            PIC 9(04).
           03  QFDB-DDIGITS-29            PIC 9(02).
           03  QFDB-DTYPE-29              PIC 9(02).
           03  QFDB-DDECIMAL-29           PIC 9(02).
           03  QFDB-DNAME-30              PIC X(08).
           03  QFDB-DOFFSET-30            PIC 9(04).
           03  QFDB-DDIGITS-30            PIC 9(02).
           03  QFDB-DTYPE-30              PIC 9(02).
           03  QFDB-DDECIMAL-30           PIC 9(02).
           03  QFDB-DNAME-31              PIC X(08).
           03  QFDB-DOFFSET-31            PIC 9(04).
           03  QFDB-DDIGITS-31            PIC 9(02).
           03  QFDB-DTYPE-31              PIC 9(02).
           03  QFDB-DDECIMAL-31           PIC 9(02).
           03  QFDB-DNAME-32              PIC X(08).
           03  QFDB-DOFFSET-32            PIC 9(04).
           03  QFDB-DDIGITS-32            PIC 9(02).
           03  QFDB-DTYPE-32              PIC 9(02).
           03  QFDB-DDECIMAL-32           PIC 9(02).
           03  QFDB-DNAME-33              PIC X(08).
           03  QFDB-DOFFSET-33            PIC 9(04).
           03  QFDB-DDIGITS-33            PIC 9(02).
           03  QFDB-DTYPE-33              PIC 9(02).
           03  QFDB-DDECIMAL-33           PIC 9(02).
           03  QFDB-DNAME-34              PIC X(08).
           03  QFDB-DOFFSET-34            PIC 9(04).
           03  QFDB-DDIGITS-34            PIC 9(02).
           03  QFDB-DTYPE-34              PIC 9(02).
           03  QFDB-DDECIMAL-34           PIC 9(02).
           03  QFDB-DNAME-35              PIC X(08).
           03  QFDB-DOFFSET-35            PIC 9(04).
           03  QFDB-DDIGITS-35            PIC 9(02).
           03  QFDB-DTYPE-35              PIC 9(02).
           03  QFDB-DDECIMAL-35           PIC 9(02).
           03  QFDB-DNAME-36              PIC X(08).
           03  QFDB-DOFFSET-36            PIC 9(04).
           03  QFDB-DDIGITS-36            PIC 9(02).
           03  QFDB-DTYPE-36              PIC 9(02).
           03  QFDB-DDECIMAL-36           PIC 9(02).
           03  QFDB-DNAME-37              PIC X(08).
           03  QFDB-DOFFSET-37            PIC 9(04).
           03  QFDB-DDIGITS-37            PIC 9(02).
           03  QFDB-DTYPE-37              PIC 9(02).
           03  QFDB-DDECIMAL-37           PIC 9(02).
           03  QFDB-DNAME-38              PIC X(08).
           03  QFDB-DOFFSET-38            PIC 9(04).
           03  QFDB-DDIGITS-38            PIC 9(02).
           03  QFDB-DTYPE-38              PIC 9(02).
           03  QFDB-DDECIMAL-38           PIC 9(02).
           03  QFDB-DNAME-39              PIC X(08).
           03  QFDB-DOFFSET-39            PIC 9(04).
           03  QFDB-DDIGITS-39            PIC 9(02).
           03  QFDB-DTYPE-39              PIC 9(02).
           03  QFDB-DDECIMAL-39           PIC 9(02).
           03  QFDB-DNAME-40              PIC X(08).
           03  QFDB-DOFFSET-40            PIC 9(04).
           03  QFDB-DDIGITS-40            PIC 9(02).
           03  QFDB-DTYPE-40              PIC 9(02).
           03  QFDB-DDECIMAL-40           PIC 9(02).
           03  QFDB-DNAME-41              PIC X(08).
           03  QFDB-DOFFSET-41            PIC 9(04).
           03  QFDB-DDIGITS-41            PIC 9(02).
           03  QFDB-DTYPE-41              PIC 9(02).
           03  QFDB-DDECIMAL-41           PIC 9(02).
           03  QFDB-DNAME-42              PIC X(08).
           03  QFDB-DOFFSET-42            PIC 9(04).
           03  QFDB-DDIGITS-42            PIC 9(02).
           03  QFDB-DTYPE-42              PIC 9(02).
           03  QFDB-DDECIMAL-42           PIC 9(02).
           03  QFDB-DNAME-43              PIC X(08).
           03  QFDB-DOFFSET-43            PIC 9(04).
           03  QFDB-DDIGITS-43            PIC 9(02).
           03  QFDB-DTYPE-43              PIC 9(02).
           03  QFDB-DDECIMAL-43           PIC 9(02).
           03  QFDB-DNAME-44              PIC X(08).
           03  QFDB-DOFFSET-44            PIC 9(04).
           03  QFDB-DDIGITS-44            PIC 9(02).
           03  QFDB-DTYPE-44              PIC 9(02).
           03  QFDB-DDECIMAL-44           PIC 9(02).
           03  QFDB-DNAME-45              PIC X(08).
           03  QFDB-DOFFSET-45            PIC 9(04).
           03  QFDB-DDIGITS-45            PIC 9(02).
           03  QFDB-DTYPE-45              PIC 9(02).
           03  QFDB-DDECIMAL-45           PIC 9(02).
           03  QFDB-DNAME-46              PIC X(08).
           03  QFDB-DOFFSET-46            PIC 9(04).
           03  QFDB-DDIGITS-46            PIC 9(02).
           03  QFDB-DTYPE-46              PIC 9(02).
           03  QFDB-DDECIMAL-46           PIC 9(02).
           03  QFDB-DNAME-47              PIC X(08).
           03  QFDB-DOFFSET-47            PIC 9(04).
           03  QFDB-DDIGITS-47            PIC 9(02).
           03  QFDB-DTYPE-47              PIC 9(02).
           03  QFDB-DDECIMAL-47           PIC 9(02).
           03  QFDB-DNAME-48              PIC X(08).
           03  QFDB-DOFFSET-48            PIC 9(04).
           03  QFDB-DDIGITS-48            PIC 9(02).
           03  QFDB-DTYPE-48              PIC 9(02).
           03  QFDB-DDECIMAL-48           PIC 9(02).
           03  QFDB-DNAME-49              PIC X(08).
           03  QFDB-DOFFSET-49            PIC 9(04).
           03  QFDB-DDIGITS-49            PIC 9(02).
           03  QFDB-DTYPE-49              PIC 9(02).
           03  QFDB-DDECIMAL-49           PIC 9(02).
           03  QFDB-DNAME-50              PIC X(08).
           03  QFDB-DOFFSET-50            PIC 9(04).
           03  QFDB-DDIGITS-50            PIC 9(02).
           03  QFDB-DTYPE-50              PIC 9(02).
           03  QFDB-DDECIMAL-50           PIC 9(02).
           03  QFDB-DNAME-51              PIC X(08).
           03  QFDB-DOFFSET-51            PIC 9(04).
           03  QFDB-DDIGITS-51            PIC 9(02).
           03  QFDB-DTYPE-51              PIC 9(02).
           03  QFDB-DDECIMAL-51           PIC 9(02).
           03  QFDB-DNAME-52              PIC X(08).
           03  QFDB-DOFFSET-52            PIC 9(04).
           03  QFDB-DDIGITS-52            PIC 9(02).
           03  QFDB-DTYPE-52              PIC 9(02).
           03  QFDB-DDECIMAL-52           PIC 9(02).
           03  QFDB-DNAME-53              PIC X(08).
           03  QFDB-DOFFSET-53            PIC 9(04).
           03  QFDB-DDIGITS-53            PIC 9(02).
           03  QFDB-DTYPE-53              PIC 9(02).
           03  QFDB-DDECIMAL-53           PIC 9(02).
           03  QFDB-DNAME-54              PIC X(08).
           03  QFDB-DOFFSET-54            PIC 9(04).
           03  QFDB-DDIGITS-54            PIC 9(02).
           03  QFDB-DTYPE-54              PIC 9(02).
           03  QFDB-DDECIMAL-54           PIC 9(02).
           03  QFDB-DNAME-55              PIC X(08).
           03  QFDB-DOFFSET-55            PIC 9(04).
           03  QFDB-DDIGITS-55            PIC 9(02).
           03  QFDB-DTYPE-55              PIC 9(02).
           03  QFDB-DDECIMAL-55           PIC 9(02).
           03  QFDB-DNAME-56              PIC X(08).
           03  QFDB-DOFFSET-56            PIC 9(04).
           03  QFDB-DDIGITS-56            PIC 9(02).
           03  QFDB-DTYPE-56              PIC 9(02).
           03  QFDB-DDECIMAL-56           PIC 9(02).
           03  QFDB-DNAME-57              PIC X(08).
           03  QFDB-DOFFSET-57            PIC 9(04).
           03  QFDB-DDIGITS-57            PIC 9(02).
           03  QFDB-DTYPE-57              PIC 9(02).
           03  QFDB-DDECIMAL-57           PIC 9(02).
           03  QFDB-DNAME-58              PIC X(08).
           03  QFDB-DOFFSET-58            PIC 9(04).
           03  QFDB-DDIGITS-58            PIC 9(02).
           03  QFDB-DTYPE-58              PIC 9(02).
           03  QFDB-DDECIMAL-58           PIC 9(02).
           03  QFDB-DNAME-59              PIC X(08).
           03  QFDB-DOFFSET-59            PIC 9(04).
           03  QFDB-DDIGITS-59            PIC 9(02).
           03  QFDB-DTYPE-59              PIC 9(02).
           03  QFDB-DDECIMAL-59           PIC 9(02).
           03  QFDB-DNAME-60              PIC X(08).
           03  QFDB-DOFFSET-60            PIC 9(04).
           03  QFDB-DDIGITS-60            PIC 9(02).
           03  QFDB-DTYPE-60              PIC 9(02).
           03  QFDB-DDECIMAL-60           PIC 9(02).
           03  QFDB-DNAME-61              PIC X(08).
           03  QFDB-DOFFSET-61            PIC 9(04).
           03  QFDB-DDIGITS-61            PIC 9(02).
           03  QFDB-DTYPE-61              PIC 9(02).
           03  QFDB-DDECIMAL-61           PIC 9(02).
           03  QFDB-DNAME-62              PIC X(08).
           03  QFDB-DOFFSET-62            PIC 9(04).
           03  QFDB-DDIGITS-62            PIC 9(02).
           03  QFDB-DTYPE-62              PIC 9(02).
           03  QFDB-DDECIMAL-62           PIC 9(02).
           03  QFDB-DNAME-63              PIC X(08).
           03  QFDB-DOFFSET-63            PIC 9(04).
           03  QFDB-DDIGITS-63            PIC 9(02).
           03  QFDB-DTYPE-63              PIC 9(02).
           03  QFDB-DDECIMAL-63           PIC 9(02).
           03  QFDB-DNAME-64              PIC X(08).
           03  QFDB-DOFFSET-64            PIC 9(04).
           03  QFDB-DDIGITS-64            PIC 9(02).
           03  QFDB-DTYPE-64              PIC 9(02).
           03  QFDB-DDECIMAL-64           PIC 9(02).
           03  QFDB-DNAME-65              PIC X(08).
           03  QFDB-DOFFSET-65            PIC 9(04).
           03  QFDB-DDIGITS-65            PIC 9(02).
           03  QFDB-DTYPE-65              PIC 9(02).
           03  QFDB-DDECIMAL-65           PIC 9(02).
           03  QFDB-DNAME-66              PIC X(08).
           03  QFDB-DOFFSET-66            PIC 9(04).
           03  QFDB-DDIGITS-66            PIC 9(02).
           03  QFDB-DTYPE-66              PIC 9(02).
           03  QFDB-DDECIMAL-66           PIC 9(02).
           03  QFDB-DNAME-67              PIC X(08).
           03  QFDB-DOFFSET-67            PIC 9(04).
           03  QFDB-DDIGITS-67            PIC 9(02).
           03  QFDB-DTYPE-67              PIC 9(02).
           03  QFDB-DDECIMAL-67           PIC 9(02).
           03  QFDB-DNAME-68              PIC X(08).
           03  QFDB-DOFFSET-68            PIC 9(04).
           03  QFDB-DDIGITS-68            PIC 9(02).
           03  QFDB-DTYPE-68              PIC 9(02).
           03  QFDB-DDECIMAL-68           PIC 9(02).
           03  QFDB-DNAME-69              PIC X(08).
           03  QFDB-DOFFSET-69            PIC 9(04).
           03  QFDB-DDIGITS-69            PIC 9(02).
           03  QFDB-DTYPE-69              PIC 9(02).
           03  QFDB-DDECIMAL-69           PIC 9(02).
           03  QFDB-DNAME-70              PIC X(08).
           03  QFDB-DOFFSET-70            PIC 9(04).
           03  QFDB-DDIGITS-70            PIC 9(02).
           03  QFDB-DTYPE-70              PIC 9(02).
           03  QFDB-DDECIMAL-70           PIC 9(02).
           03  QFDB-DNAME-71              PIC X(08).
           03  QFDB-DOFFSET-71            PIC 9(04).
           03  QFDB-DDIGITS-71            PIC 9(02).
           03  QFDB-DTYPE-71              PIC 9(02).
           03  QFDB-DDECIMAL-71           PIC 9(02).
           03  QFDB-DNAME-72              PIC X(08).
           03  QFDB-DOFFSET-72            PIC 9(04).
           03  QFDB-DDIGITS-72            PIC 9(02).
           03  QFDB-DTYPE-72              PIC 9(02).
           03  QFDB-DDECIMAL-72           PIC 9(02).
           03  QFDB-DNAME-73              PIC X(08).
           03  QFDB-DOFFSET-73            PIC 9(04).
           03  QFDB-DDIGITS-73            PIC 9(02).
           03  QFDB-DTYPE-73              PIC 9(02).
           03  QFDB-DDECIMAL-73           PIC 9(02).
           03  QFDB-DNAME-74              PIC X(08).
           03  QFDB-DOFFSET-74            PIC 9(04).
           03  QFDB-DDIGITS-74            PIC 9(02).
           03  QFDB-DTYPE-74              PIC 9(02).
           03  QFDB-DDECIMAL-74           PIC 9(02).
           03  QFDB-DNAME-75              PIC X(08).
           03  QFDB-DOFFSET-75            PIC 9(04).
           03  QFDB-DDIGITS-75            PIC 9(02).
           03  QFDB-DTYPE-75              PIC 9(02).
           03  QFDB-DDECIMAL-75           PIC 9(02).
           03  QFDB-DNAME-76              PIC X(08).
           03  QFDB-DOFFSET-76            PIC 9(04).
           03  QFDB-DDIGITS-76            PIC 9(02).
           03  QFDB-DTYPE-76              PIC 9(02).
           03  QFDB-DDECIMAL-76           PIC 9(02).
           03  QFDB-DNAME-77              PIC X(08).
           03  QFDB-DOFFSET-77            PIC 9(04).
           03  QFDB-DDIGITS-77            PIC 9(02).
           03  QFDB-DTYPE-77              PIC 9(02).
           03  QFDB-DDECIMAL-77           PIC 9(02).
           03  QFDB-DNAME-78              PIC X(08).
           03  QFDB-DOFFSET-78            PIC 9(04).
           03  QFDB-DDIGITS-78            PIC 9(02).
           03  QFDB-DTYPE-78              PIC 9(02).
           03  QFDB-DDECIMAL-78           PIC 9(02).
           03  QFDB-DNAME-79              PIC X(08).
           03  QFDB-DOFFSET-79            PIC 9(04).
           03  QFDB-DDIGITS-79            PIC 9(02).
           03  QFDB-DTYPE-79              PIC 9(02).
           03  QFDB-DDECIMAL-79           PIC 9(02).
           03  QFDB-DNAME-80              PIC X(08).
           03  QFDB-DOFFSET-80            PIC 9(04).
           03  QFDB-DDIGITS-80            PIC 9(02).
           03  QFDB-DTYPE-80              PIC 9(02).
           03  QFDB-DDECIMAL-80           PIC 9(02).
           03  QFDB-DNAME-81              PIC X(08).
           03  QFDB-DOFFSET-81            PIC 9(04).
           03  QFDB-DDIGITS-81            PIC 9(02).
           03  QFDB-DTYPE-81              PIC 9(02).
           03  QFDB-DDECIMAL-81           PIC 9(02).
           03  QFDB-DNAME-82              PIC X(08).
           03  QFDB-DOFFSET-82            PIC 9(04).
           03  QFDB-DDIGITS-82            PIC 9(02).
           03  QFDB-DTYPE-82              PIC 9(02).
           03  QFDB-DDECIMAL-82           PIC 9(02).
           03  QFDB-DNAME-83              PIC X(08).
           03  QFDB-DOFFSET-83            PIC 9(04).
           03  QFDB-DDIGITS-83            PIC 9(02).
           03  QFDB-DTYPE-83              PIC 9(02).
           03  QFDB-DDECIMAL-83           PIC 9(02).
           03  QFDB-DNAME-84              PIC X(08).
           03  QFDB-DOFFSET-84            PIC 9(04).
           03  QFDB-DDIGITS-84            PIC 9(02).
           03  QFDB-DTYPE-84              PIC 9(02).
           03  QFDB-DDECIMAL-84           PIC 9(02).
           03  QFDB-DNAME-85              PIC X(08).
           03  QFDB-DOFFSET-85            PIC 9(04).
           03  QFDB-DDIGITS-85            PIC 9(02).
           03  QFDB-DTYPE-85              PIC 9(02).
           03  QFDB-DDECIMAL-85           PIC 9(02).
           03  QFDB-DNAME-86              PIC X(08).
           03  QFDB-DOFFSET-86            PIC 9(04).
           03  QFDB-DDIGITS-86            PIC 9(02).
           03  QFDB-DTYPE-86              PIC 9(02).
           03  QFDB-DDECIMAL-86           PIC 9(02).
           03  QFDB-DNAME-87              PIC X(08).
           03  QFDB-DOFFSET-87            PIC 9(04).
           03  QFDB-DDIGITS-87            PIC 9(02).
           03  QFDB-DTYPE-87              PIC 9(02).
           03  QFDB-DDECIMAL-87           PIC 9(02).
           03  QFDB-DNAME-88              PIC X(08).
           03  QFDB-DOFFSET-88            PIC 9(04).
           03  QFDB-DDIGITS-88            PIC 9(02).
           03  QFDB-DTYPE-88              PIC 9(02).
           03  QFDB-DDECIMAL-88           PIC 9(02).
           03  QFDB-DNAME-89              PIC X(08).
           03  QFDB-DOFFSET-89            PIC 9(04).
           03  QFDB-DDIGITS-89            PIC 9(02).
           03  QFDB-DTYPE-89              PIC 9(02).
           03  QFDB-DDECIMAL-89           PIC 9(02).
           03  QFDB-DNAME-90              PIC X(08).
           03  QFDB-DOFFSET-90            PIC 9(04).
           03  QFDB-DDIGITS-90            PIC 9(02).
           03  QFDB-DTYPE-90              PIC 9(02).
           03  QFDB-DDECIMAL-90           PIC 9(02).
           03  QFDB-DNAME-91              PIC X(08).
           03  QFDB-DOFFSET-91            PIC 9(04).
           03  QFDB-DDIGITS-91            PIC 9(02).
           03  QFDB-DTYPE-91              PIC 9(02).
           03  QFDB-DDECIMAL-91           PIC 9(02).
           03  QFDB-DNAME-92              PIC X(08).
           03  QFDB-DOFFSET-92            PIC 9(04).
           03  QFDB-DDIGITS-92            PIC 9(02).
           03  QFDB-DTYPE-92              PIC 9(02).
           03  QFDB-DDECIMAL-92           PIC 9(02).
           03  QFDB-DNAME-93              PIC X(08).
           03  QFDB-DOFFSET-93            PIC 9(04).
           03  QFDB-DDIGITS-93            PIC 9(02).
           03  QFDB-DTYPE-93              PIC 9(02).
           03  QFDB-DDECIMAL-93           PIC 9(02).
           03  QFDB-DNAME-94              PIC X(08).
           03  QFDB-DOFFSET-94            PIC 9(04).
           03  QFDB-DDIGITS-94            PIC 9(02).
           03  QFDB-DTYPE-94              PIC 9(02).
           03  QFDB-DDECIMAL-94           PIC 9(02).
           03  QFDB-DNAME-95              PIC X(08).
           03  QFDB-DOFFSET-95            PIC 9(04).
           03  QFDB-DDIGITS-95            PIC 9(02).
           03  QFDB-DTYPE-95              PIC 9(02).
           03  QFDB-DDECIMAL-95           PIC 9(02).
           03  QFDB-DNAME-96              PIC X(08).
           03  QFDB-DOFFSET-96            PIC 9(04).
           03  QFDB-DDIGITS-96            PIC 9(02).
           03  QFDB-DTYPE-96              PIC 9(02).
           03  QFDB-DDECIMAL-96           PIC 9(02).
           03  QFDB-DNAME-97              PIC X(08).
           03  QFDB-DOFFSET-97            PIC 9(04).
           03  QFDB-DDIGITS-97            PIC 9(02).
           03  QFDB-DTYPE-97              PIC 9(02).
           03  QFDB-DDECIMAL-97           PIC 9(02).
           03  QFDB-DNAME-98              PIC X(08).
           03  QFDB-DOFFSET-98            PIC 9(04).
           03  QFDB-DDIGITS-98            PIC 9(02).
           03  QFDB-DTYPE-98              PIC 9(02).
           03  QFDB-DDECIMAL-98           PIC 9(02).
           03  QFDB-DNAME-99              PIC X(08).
           03  QFDB-DOFFSET-99            PIC 9(04).
           03  QFDB-DDIGITS-99            PIC 9(02).
           03  QFDB-DTYPE-99              PIC 9(02).
           03  QFDB-DDECIMAL-99           PIC 9(02).
           03  QFDB-DNAME-100              PIC X(08).
           03  QFDB-DOFFSET-100             PIC 9(04).
           03  QFDB-DDIGITS-100             PIC 9(02).
           03  QFDB-DTYPE-100               PIC 9(02).
           03  QFDB-DDECIMAL-100            PIC 9(02).
           03  QFDB-DNAME-101              PIC X(08).
           03  QFDB-DOFFSET-101             PIC 9(04).
           03  QFDB-DDIGITS-101             PIC 9(02).
           03  QFDB-DTYPE-101               PIC 9(02).
           03  QFDB-DDECIMAL-101            PIC 9(02).
           03  QFDB-DNAME-102              PIC X(08).
           03  QFDB-DOFFSET-102             PIC 9(04).
           03  QFDB-DDIGITS-102             PIC 9(02).
           03  QFDB-DTYPE-102               PIC 9(02).
           03  QFDB-DDECIMAL-102            PIC 9(02).
           03  QFDB-DNAME-103              PIC X(08).
           03  QFDB-DOFFSET-103             PIC 9(04).
           03  QFDB-DDIGITS-103             PIC 9(02).
           03  QFDB-DTYPE-103               PIC 9(02).
           03  QFDB-DDECIMAL-103            PIC 9(02).
           03  QFDB-DNAME-104              PIC X(08).
           03  QFDB-DOFFSET-104             PIC 9(04).
           03  QFDB-DDIGITS-104             PIC 9(02).
           03  QFDB-DTYPE-104               PIC 9(02).
           03  QFDB-DDECIMAL-104            PIC 9(02).
           03  QFDB-DNAME-105              PIC X(08).
           03  QFDB-DOFFSET-105             PIC 9(04).
           03  QFDB-DDIGITS-105             PIC 9(02).
           03  QFDB-DTYPE-105               PIC 9(02).
           03  QFDB-DDECIMAL-105            PIC 9(02).
           03  QFDB-DNAME-106              PIC X(08).
           03  QFDB-DOFFSET-106             PIC 9(04).
           03  QFDB-DDIGITS-106             PIC 9(02).
           03  QFDB-DTYPE-106               PIC 9(02).
           03  QFDB-DDECIMAL-106            PIC 9(02).
           03  QFDB-DNAME-107              PIC X(08).
           03  QFDB-DOFFSET-107             PIC 9(04).
           03  QFDB-DDIGITS-107             PIC 9(02).
           03  QFDB-DTYPE-107               PIC 9(02).
           03  QFDB-DDECIMAL-107            PIC 9(02).
           03  QFDB-DNAME-108              PIC X(08).
           03  QFDB-DOFFSET-108             PIC 9(04).
           03  QFDB-DDIGITS-108             PIC 9(02).
           03  QFDB-DTYPE-108               PIC 9(02).
           03  QFDB-DDECIMAL-108            PIC 9(02).
           03  QFDB-DNAME-109              PIC X(08).
           03  QFDB-DOFFSET-109             PIC 9(04).
           03  QFDB-DDIGITS-109             PIC 9(02).
           03  QFDB-DTYPE-109               PIC 9(02).
           03  QFDB-DDECIMAL-109            PIC 9(02).
           03  QFDB-DNAME-110              PIC X(08).
           03  QFDB-DOFFSET-110            PIC 9(04).
           03  QFDB-DDIGITS-110            PIC 9(02).
           03  QFDB-DTYPE-110              PIC 9(02).
           03  QFDB-DDECIMAL-110           PIC 9(02).
           03  QFDB-DNAME-111              PIC X(08).
           03  QFDB-DOFFSET-111            PIC 9(04).
           03  QFDB-DDIGITS-111            PIC 9(02).
           03  QFDB-DTYPE-111              PIC 9(02).
           03  QFDB-DDECIMAL-111           PIC 9(02).
           03  QFDB-DNAME-112              PIC X(08).
           03  QFDB-DOFFSET-112            PIC 9(04).
           03  QFDB-DDIGITS-112            PIC 9(02).
           03  QFDB-DTYPE-112              PIC 9(02).
           03  QFDB-DDECIMAL-112           PIC 9(02).
           03  QFDB-DNAME-113              PIC X(08).
           03  QFDB-DOFFSET-113            PIC 9(04).
           03  QFDB-DDIGITS-113            PIC 9(02).
           03  QFDB-DTYPE-113              PIC 9(02).
           03  QFDB-DDECIMAL-113           PIC 9(02).
           03  QFDB-DNAME-114              PIC X(08).
           03  QFDB-DOFFSET-114            PIC 9(04).
           03  QFDB-DDIGITS-114            PIC 9(02).
           03  QFDB-DTYPE-114              PIC 9(02).
           03  QFDB-DDECIMAL-114           PIC 9(02).
           03  QFDB-DNAME-115              PIC X(08).
           03  QFDB-DOFFSET-115            PIC 9(04).
           03  QFDB-DDIGITS-115            PIC 9(02).
           03  QFDB-DTYPE-115              PIC 9(02).
           03  QFDB-DDECIMAL-115           PIC 9(02).
           03  QFDB-DNAME-116              PIC X(08).
           03  QFDB-DOFFSET-116            PIC 9(04).
           03  QFDB-DDIGITS-116            PIC 9(02).
           03  QFDB-DTYPE-116              PIC 9(02).
           03  QFDB-DDECIMAL-116           PIC 9(02).
           03  QFDB-DNAME-117              PIC X(08).
           03  QFDB-DOFFSET-117            PIC 9(04).
           03  QFDB-DDIGITS-117            PIC 9(02).
           03  QFDB-DTYPE-117              PIC 9(02).
           03  QFDB-DDECIMAL-117           PIC 9(02).
           03  QFDB-DNAME-118              PIC X(08).
           03  QFDB-DOFFSET-118            PIC 9(04).
           03  QFDB-DDIGITS-118            PIC 9(02).
           03  QFDB-DTYPE-118              PIC 9(02).
           03  QFDB-DDECIMAL-118           PIC 9(02).
           03  QFDB-DNAME-119              PIC X(08).
           03  QFDB-DOFFSET-119            PIC 9(04).
           03  QFDB-DDIGITS-119            PIC 9(02).
           03  QFDB-DTYPE-119              PIC 9(02).
           03  QFDB-DDECIMAL-119           PIC 9(02).
           03  QFDB-DNAME-120              PIC X(08).
           03  QFDB-DOFFSET-120            PIC 9(04).
           03  QFDB-DDIGITS-120            PIC 9(02).
           03  QFDB-DTYPE-120              PIC 9(02).
           03  QFDB-DDECIMAL-120           PIC 9(02).
           03  QFDB-DNAME-121              PIC X(08).
           03  QFDB-DOFFSET-121            PIC 9(04).
           03  QFDB-DDIGITS-121            PIC 9(02).
           03  QFDB-DTYPE-121              PIC 9(02).
           03  QFDB-DDECIMAL-121           PIC 9(02).
           03  QFDB-DNAME-122              PIC X(08).
           03  QFDB-DOFFSET-122            PIC 9(04).
           03  QFDB-DDIGITS-122            PIC 9(02).
           03  QFDB-DTYPE-122              PIC 9(02).
           03  QFDB-DDECIMAL-122           PIC 9(02).
           03  QFDB-DNAME-123              PIC X(08).
           03  QFDB-DOFFSET-123            PIC 9(04).
           03  QFDB-DDIGITS-123            PIC 9(02).
           03  QFDB-DTYPE-123              PIC 9(02).
           03  QFDB-DDECIMAL-123           PIC 9(02).
           03  QFDB-DNAME-124              PIC X(08).
           03  QFDB-DOFFSET-124            PIC 9(04).
           03  QFDB-DDIGITS-124            PIC 9(02).
           03  QFDB-DTYPE-124              PIC 9(02).
           03  QFDB-DDECIMAL-124           PIC 9(02).
           03  QFDB-DNAME-125              PIC X(08).
           03  QFDB-DOFFSET-125            PIC 9(04).
           03  QFDB-DDIGITS-125            PIC 9(02).
           03  QFDB-DTYPE-125              PIC 9(02).
           03  QFDB-DDECIMAL-125           PIC 9(02).
           03  QFDB-DNAME-126              PIC X(08).
           03  QFDB-DOFFSET-126            PIC 9(04).
           03  QFDB-DDIGITS-126            PIC 9(02).
           03  QFDB-DTYPE-126              PIC 9(02).
           03  QFDB-DDECIMAL-126           PIC 9(02).
           03  QFDB-DNAME-127              PIC X(08).
           03  QFDB-DOFFSET-127            PIC 9(04).
           03  QFDB-DDIGITS-127            PIC 9(02).
           03  QFDB-DTYPE-127              PIC 9(02).
           03  QFDB-DDECIMAL-127           PIC 9(02).
           03  QFDB-DNAME-128              PIC X(08).
           03  QFDB-DOFFSET-128            PIC 9(04).
           03  QFDB-DDIGITS-128            PIC 9(02).
           03  QFDB-DTYPE-128              PIC 9(02).
           03  QFDB-DDECIMAL-128           PIC 9(02).
           03  QFDB-DNAME-129              PIC X(08).
           03  QFDB-DOFFSET-129            PIC 9(04).
           03  QFDB-DDIGITS-129            PIC 9(02).
           03  QFDB-DTYPE-129              PIC 9(02).
           03  QFDB-DDECIMAL-129           PIC 9(02).
           03  QFDB-DNAME-130              PIC X(08).
           03  QFDB-DOFFSET-130            PIC 9(04).
           03  QFDB-DDIGITS-130            PIC 9(02).
           03  QFDB-DTYPE-130              PIC 9(02).
           03  QFDB-DDECIMAL-130           PIC 9(02).
           03  QFDB-DNAME-131              PIC X(08).
           03  QFDB-DOFFSET-131            PIC 9(04).
           03  QFDB-DDIGITS-131            PIC 9(02).
           03  QFDB-DTYPE-131              PIC 9(02).
           03  QFDB-DDECIMAL-131           PIC 9(02).
           03  QFDB-DNAME-132              PIC X(08).
           03  QFDB-DOFFSET-132            PIC 9(04).
           03  QFDB-DDIGITS-132            PIC 9(02).
           03  QFDB-DTYPE-132              PIC 9(02).
           03  QFDB-DDECIMAL-132           PIC 9(02).
           03  QFDB-DNAME-133              PIC X(08).
           03  QFDB-DOFFSET-133            PIC 9(04).
           03  QFDB-DDIGITS-133            PIC 9(02).
           03  QFDB-DTYPE-133              PIC 9(02).
           03  QFDB-DDECIMAL-133           PIC 9(02).
           03  QFDB-DNAME-134              PIC X(08).
           03  QFDB-DOFFSET-134            PIC 9(04).
           03  QFDB-DDIGITS-134            PIC 9(02).
           03  QFDB-DTYPE-134              PIC 9(02).
           03  QFDB-DDECIMAL-134           PIC 9(02).
           03  QFDB-DNAME-135              PIC X(08).
           03  QFDB-DOFFSET-135            PIC 9(04).
           03  QFDB-DDIGITS-135            PIC 9(02).
           03  QFDB-DTYPE-135              PIC 9(02).
           03  QFDB-DDECIMAL-135           PIC 9(02).
           03  QFDB-DNAME-136              PIC X(08).
           03  QFDB-DOFFSET-136            PIC 9(04).
           03  QFDB-DDIGITS-136            PIC 9(02).
           03  QFDB-DTYPE-136              PIC 9(02).
           03  QFDB-DDECIMAL-136           PIC 9(02).
           03  QFDB-DNAME-137              PIC X(08).
           03  QFDB-DOFFSET-137            PIC 9(04).
           03  QFDB-DDIGITS-137            PIC 9(02).
           03  QFDB-DTYPE-137              PIC 9(02).
           03  QFDB-DDECIMAL-137           PIC 9(02).
           03  QFDB-DNAME-138              PIC X(08).
           03  QFDB-DOFFSET-138            PIC 9(04).
           03  QFDB-DDIGITS-138            PIC 9(02).
           03  QFDB-DTYPE-138              PIC 9(02).
           03  QFDB-DDECIMAL-138           PIC 9(02).
           03  QFDB-DNAME-139              PIC X(08).
           03  QFDB-DOFFSET-139            PIC 9(04).
           03  QFDB-DDIGITS-139            PIC 9(02).
           03  QFDB-DTYPE-139              PIC 9(02).
           03  QFDB-DDECIMAL-139           PIC 9(02).
           03  QFDB-DNAME-140              PIC X(08).
           03  QFDB-DOFFSET-140            PIC 9(04).
           03  QFDB-DDIGITS-140            PIC 9(02).
           03  QFDB-DTYPE-140              PIC 9(02).
           03  QFDB-DDECIMAL-140           PIC 9(02).
           03  QFDB-DNAME-141              PIC X(08).
           03  QFDB-DOFFSET-141            PIC 9(04).
           03  QFDB-DDIGITS-141            PIC 9(02).
           03  QFDB-DTYPE-141              PIC 9(02).
           03  QFDB-DDECIMAL-141           PIC 9(02).
           03  QFDB-DNAME-142              PIC X(08).
           03  QFDB-DOFFSET-142            PIC 9(04).
           03  QFDB-DDIGITS-142            PIC 9(02).
           03  QFDB-DTYPE-142              PIC 9(02).
           03  QFDB-DDECIMAL-142           PIC 9(02).
           03  QFDB-DNAME-143              PIC X(08).
           03  QFDB-DOFFSET-143            PIC 9(04).
           03  QFDB-DDIGITS-143            PIC 9(02).
           03  QFDB-DTYPE-143              PIC 9(02).
           03  QFDB-DDECIMAL-143           PIC 9(02).
           03  QFDB-DNAME-144              PIC X(08).
           03  QFDB-DOFFSET-144            PIC 9(04).
           03  QFDB-DDIGITS-144            PIC 9(02).
           03  QFDB-DTYPE-144              PIC 9(02).
           03  QFDB-DDECIMAL-144           PIC 9(02).
           03  QFDB-DNAME-145              PIC X(08).
           03  QFDB-DOFFSET-145            PIC 9(04).
           03  QFDB-DDIGITS-145            PIC 9(02).
           03  QFDB-DTYPE-145              PIC 9(02).
           03  QFDB-DDECIMAL-145           PIC 9(02).
           03  QFDB-DNAME-146              PIC X(08).
           03  QFDB-DOFFSET-146            PIC 9(04).
           03  QFDB-DDIGITS-146            PIC 9(02).
           03  QFDB-DTYPE-146              PIC 9(02).
           03  QFDB-DDECIMAL-146           PIC 9(02).
           03  QFDB-DNAME-147              PIC X(08).
           03  QFDB-DOFFSET-147            PIC 9(04).
           03  QFDB-DDIGITS-147            PIC 9(02).
           03  QFDB-DTYPE-147              PIC 9(02).
           03  QFDB-DDECIMAL-147           PIC 9(02).
           03  QFDB-DNAME-148              PIC X(08).
           03  QFDB-DOFFSET-148            PIC 9(04).
           03  QFDB-DDIGITS-148            PIC 9(02).
           03  QFDB-DTYPE-148              PIC 9(02).
           03  QFDB-DDECIMAL-148           PIC 9(02).
           03  QFDB-DNAME-149              PIC X(08).
           03  QFDB-DOFFSET-149            PIC 9(04).
           03  QFDB-DDIGITS-149            PIC 9(02).
           03  QFDB-DTYPE-149              PIC 9(02).
           03  QFDB-DDECIMAL-149           PIC 9(02).
           03  QFDB-DNAME-150              PIC X(08).
           03  QFDB-DOFFSET-150            PIC 9(04).
           03  QFDB-DDIGITS-150            PIC 9(02).
           03  QFDB-DTYPE-150              PIC 9(02).
           03  QFDB-DDECIMAL-150           PIC 9(02).
           03  QFDB-DNAME-151              PIC X(08).
           03  QFDB-DOFFSET-151            PIC 9(04).
           03  QFDB-DDIGITS-151            PIC 9(02).
           03  QFDB-DTYPE-151              PIC 9(02).
           03  QFDB-DDECIMAL-151           PIC 9(02).
           03  QFDB-DNAME-152              PIC X(08).
           03  QFDB-DOFFSET-152            PIC 9(04).
           03  QFDB-DDIGITS-152            PIC 9(02).
           03  QFDB-DTYPE-152              PIC 9(02).
           03  QFDB-DDECIMAL-152           PIC 9(02).
           03  QFDB-DNAME-153              PIC X(08).
           03  QFDB-DOFFSET-153            PIC 9(04).
           03  QFDB-DDIGITS-153            PIC 9(02).
           03  QFDB-DTYPE-153              PIC 9(02).
           03  QFDB-DDECIMAL-153           PIC 9(02).
           03  QFDB-DNAME-154              PIC X(08).
           03  QFDB-DOFFSET-154            PIC 9(04).
           03  QFDB-DDIGITS-154            PIC 9(02).
           03  QFDB-DTYPE-154              PIC 9(02).
           03  QFDB-DDECIMAL-154           PIC 9(02).
           03  QFDB-DNAME-155              PIC X(08).
           03  QFDB-DOFFSET-155            PIC 9(04).
           03  QFDB-DDIGITS-155            PIC 9(02).
           03  QFDB-DTYPE-155              PIC 9(02).
           03  QFDB-DDECIMAL-155           PIC 9(02).
           03  QFDB-DNAME-156              PIC X(08).
           03  QFDB-DOFFSET-156            PIC 9(04).
           03  QFDB-DDIGITS-156            PIC 9(02).
           03  QFDB-DTYPE-156              PIC 9(02).
           03  QFDB-DDECIMAL-156           PIC 9(02).
           03  QFDB-DNAME-157              PIC X(08).
           03  QFDB-DOFFSET-157            PIC 9(04).
           03  QFDB-DDIGITS-157            PIC 9(02).
           03  QFDB-DTYPE-157              PIC 9(02).
           03  QFDB-DDECIMAL-157           PIC 9(02).
           03  QFDB-DNAME-158              PIC X(08).
           03  QFDB-DOFFSET-158            PIC 9(04).
           03  QFDB-DDIGITS-158            PIC 9(02).
           03  QFDB-DTYPE-158              PIC 9(02).
           03  QFDB-DDECIMAL-158           PIC 9(02).
           03  QFDB-DNAME-159              PIC X(08).
           03  QFDB-DOFFSET-159            PIC 9(04).
           03  QFDB-DDIGITS-159            PIC 9(02).
           03  QFDB-DTYPE-159              PIC 9(02).
           03  QFDB-DDECIMAL-159           PIC 9(02).
           03  QFDB-DNAME-160              PIC X(08).
           03  QFDB-DOFFSET-160            PIC 9(04).
           03  QFDB-DDIGITS-160            PIC 9(02).
           03  QFDB-DTYPE-160              PIC 9(02).
           03  QFDB-DDECIMAL-160           PIC 9(02).
           03  QFDB-DNAME-161              PIC X(08).
           03  QFDB-DOFFSET-161            PIC 9(04).
           03  QFDB-DDIGITS-161            PIC 9(02).
           03  QFDB-DTYPE-161              PIC 9(02).
           03  QFDB-DDECIMAL-161           PIC 9(02).
           03  QFDB-DNAME-162              PIC X(08).
           03  QFDB-DOFFSET-162            PIC 9(04).
           03  QFDB-DDIGITS-162            PIC 9(02).
           03  QFDB-DTYPE-162              PIC 9(02).
           03  QFDB-DDECIMAL-162           PIC 9(02).
           03  QFDB-DNAME-163              PIC X(08).
           03  QFDB-DOFFSET-163            PIC 9(04).
           03  QFDB-DDIGITS-163            PIC 9(02).
           03  QFDB-DTYPE-163              PIC 9(02).
           03  QFDB-DDECIMAL-163           PIC 9(02).
           03  QFDB-DNAME-164              PIC X(08).
           03  QFDB-DOFFSET-164            PIC 9(04).
           03  QFDB-DDIGITS-164            PIC 9(02).
           03  QFDB-DTYPE-164              PIC 9(02).
           03  QFDB-DDECIMAL-164           PIC 9(02).
           03  QFDB-DNAME-165              PIC X(08).
           03  QFDB-DOFFSET-165            PIC 9(04).
           03  QFDB-DDIGITS-165            PIC 9(02).
           03  QFDB-DTYPE-165              PIC 9(02).
           03  QFDB-DDECIMAL-165           PIC 9(02).
           03  QFDB-DNAME-166              PIC X(08).
           03  QFDB-DOFFSET-166            PIC 9(04).
           03  QFDB-DDIGITS-166            PIC 9(02).
           03  QFDB-DTYPE-166              PIC 9(02).
           03  QFDB-DDECIMAL-166           PIC 9(02).
           03  QFDB-DNAME-167              PIC X(08).
           03  QFDB-DOFFSET-167            PIC 9(04).
           03  QFDB-DDIGITS-167            PIC 9(02).
           03  QFDB-DTYPE-167              PIC 9(02).
           03  QFDB-DDECIMAL-167           PIC 9(02).
           03  QFDB-DNAME-168              PIC X(08).
           03  QFDB-DOFFSET-168            PIC 9(04).
           03  QFDB-DDIGITS-168            PIC 9(02).
           03  QFDB-DTYPE-168              PIC 9(02).
           03  QFDB-DDECIMAL-168           PIC 9(02).
           03  QFDB-DNAME-169              PIC X(08).
           03  QFDB-DOFFSET-169            PIC 9(04).
           03  QFDB-DDIGITS-169            PIC 9(02).
           03  QFDB-DTYPE-169              PIC 9(02).
           03  QFDB-DDECIMAL-169           PIC 9(02).
           03  QFDB-DNAME-170              PIC X(08).
           03  QFDB-DOFFSET-170            PIC 9(04).
           03  QFDB-DDIGITS-170            PIC 9(02).
           03  QFDB-DTYPE-170              PIC 9(02).
           03  QFDB-DDECIMAL-170           PIC 9(02).
           03  QFDB-DNAME-171              PIC X(08).
           03  QFDB-DOFFSET-171            PIC 9(04).
           03  QFDB-DDIGITS-171            PIC 9(02).
           03  QFDB-DTYPE-171              PIC 9(02).
           03  QFDB-DDECIMAL-171           PIC 9(02).
           03  QFDB-DNAME-172              PIC X(08).
           03  QFDB-DOFFSET-172            PIC 9(04).
           03  QFDB-DDIGITS-172            PIC 9(02).
           03  QFDB-DTYPE-172              PIC 9(02).
           03  QFDB-DDECIMAL-172           PIC 9(02).
           03  QFDB-DNAME-173              PIC X(08).
           03  QFDB-DOFFSET-173            PIC 9(04).
           03  QFDB-DDIGITS-173            PIC 9(02).
           03  QFDB-DTYPE-173              PIC 9(02).
           03  QFDB-DDECIMAL-173           PIC 9(02).
           03  QFDB-DNAME-174              PIC X(08).
           03  QFDB-DOFFSET-174            PIC 9(04).
           03  QFDB-DDIGITS-174            PIC 9(02).
           03  QFDB-DTYPE-174              PIC 9(02).
           03  QFDB-DDECIMAL-174           PIC 9(02).
           03  QFDB-DNAME-175              PIC X(08).
           03  QFDB-DOFFSET-175            PIC 9(04).
           03  QFDB-DDIGITS-175            PIC 9(02).
           03  QFDB-DTYPE-175              PIC 9(02).
           03  QFDB-DDECIMAL-175           PIC 9(02).
           03  QFDB-DNAME-176              PIC X(08).
           03  QFDB-DOFFSET-176            PIC 9(04).
           03  QFDB-DDIGITS-176            PIC 9(02).
           03  QFDB-DTYPE-176              PIC 9(02).
           03  QFDB-DDECIMAL-176           PIC 9(02).
           03  QFDB-DNAME-177              PIC X(08).
           03  QFDB-DOFFSET-177            PIC 9(04).
           03  QFDB-DDIGITS-177            PIC 9(02).
           03  QFDB-DTYPE-177              PIC 9(02).
           03  QFDB-DDECIMAL-177           PIC 9(02).
           03  QFDB-DNAME-178              PIC X(08).
           03  QFDB-DOFFSET-178            PIC 9(04).
           03  QFDB-DDIGITS-178            PIC 9(02).
           03  QFDB-DTYPE-178              PIC 9(02).
           03  QFDB-DDECIMAL-178           PIC 9(02).
           03  QFDB-DNAME-179              PIC X(08).
           03  QFDB-DOFFSET-179            PIC 9(04).
           03  QFDB-DDIGITS-179            PIC 9(02).
           03  QFDB-DTYPE-179              PIC 9(02).
           03  QFDB-DDECIMAL-179           PIC 9(02).
           03  QFDB-DNAME-180              PIC X(08).
           03  QFDB-DOFFSET-180            PIC 9(04).
           03  QFDB-DDIGITS-180            PIC 9(02).
           03  QFDB-DTYPE-180              PIC 9(02).
           03  QFDB-DDECIMAL-180           PIC 9(02).
           03  QFDB-DNAME-181              PIC X(08).
           03  QFDB-DOFFSET-181            PIC 9(04).
           03  QFDB-DDIGITS-181            PIC 9(02).
           03  QFDB-DTYPE-181              PIC 9(02).
           03  QFDB-DDECIMAL-181           PIC 9(02).
           03  QFDB-DNAME-182              PIC X(08).
           03  QFDB-DOFFSET-182            PIC 9(04).
           03  QFDB-DDIGITS-182            PIC 9(02).
           03  QFDB-DTYPE-182              PIC 9(02).
           03  QFDB-DDECIMAL-182           PIC 9(02).
           03  QFDB-DNAME-183              PIC X(08).
           03  QFDB-DOFFSET-183            PIC 9(04).
           03  QFDB-DDIGITS-183            PIC 9(02).
           03  QFDB-DTYPE-183              PIC 9(02).
           03  QFDB-DDECIMAL-183           PIC 9(02).
           03  QFDB-DNAME-184              PIC X(08).
           03  QFDB-DOFFSET-184            PIC 9(04).
           03  QFDB-DDIGITS-184            PIC 9(02).
           03  QFDB-DTYPE-184              PIC 9(02).
           03  QFDB-DDECIMAL-184           PIC 9(02).
           03  QFDB-DNAME-185              PIC X(08).
           03  QFDB-DOFFSET-185            PIC 9(04).
           03  QFDB-DDIGITS-185            PIC 9(02).
           03  QFDB-DTYPE-185              PIC 9(02).
           03  QFDB-DDECIMAL-185           PIC 9(02).
           03  QFDB-DNAME-186              PIC X(08).
           03  QFDB-DOFFSET-186            PIC 9(04).
           03  QFDB-DDIGITS-186            PIC 9(02).
           03  QFDB-DTYPE-186              PIC 9(02).
           03  QFDB-DDECIMAL-186           PIC 9(02).
           03  QFDB-DNAME-187              PIC X(08).
           03  QFDB-DOFFSET-187            PIC 9(04).
           03  QFDB-DDIGITS-187            PIC 9(02).
           03  QFDB-DTYPE-187              PIC 9(02).
           03  QFDB-DDECIMAL-187           PIC 9(02).
           03  QFDB-DNAME-188              PIC X(08).
           03  QFDB-DOFFSET-188            PIC 9(04).
           03  QFDB-DDIGITS-188            PIC 9(02).
           03  QFDB-DTYPE-188              PIC 9(02).
           03  QFDB-DDECIMAL-188           PIC 9(02).
           03  QFDB-DNAME-189              PIC X(08).
           03  QFDB-DOFFSET-189            PIC 9(04).
           03  QFDB-DDIGITS-189            PIC 9(02).
           03  QFDB-DTYPE-189              PIC 9(02).
           03  QFDB-DDECIMAL-189           PIC 9(02).
           03  QFDB-DNAME-190              PIC X(08).
           03  QFDB-DOFFSET-190            PIC 9(04).
           03  QFDB-DDIGITS-190            PIC 9(02).
           03  QFDB-DTYPE-190              PIC 9(02).
           03  QFDB-DDECIMAL-190           PIC 9(02).
           03  QFDB-DNAME-191              PIC X(08).
           03  QFDB-DOFFSET-191            PIC 9(04).
           03  QFDB-DDIGITS-191            PIC 9(02).
           03  QFDB-DTYPE-191              PIC 9(02).
           03  QFDB-DDECIMAL-191           PIC 9(02).
           03  QFDB-DNAME-192              PIC X(08).
           03  QFDB-DOFFSET-192            PIC 9(04).
           03  QFDB-DDIGITS-192            PIC 9(02).
           03  QFDB-DTYPE-192              PIC 9(02).
           03  QFDB-DDECIMAL-192           PIC 9(02).
           03  QFDB-DNAME-193              PIC X(08).
           03  QFDB-DOFFSET-193            PIC 9(04).
           03  QFDB-DDIGITS-193            PIC 9(02).
           03  QFDB-DTYPE-193              PIC 9(02).
           03  QFDB-DDECIMAL-193           PIC 9(02).
           03  QFDB-DNAME-194              PIC X(08).
           03  QFDB-DOFFSET-194            PIC 9(04).
           03  QFDB-DDIGITS-194            PIC 9(02).
           03  QFDB-DTYPE-194              PIC 9(02).
           03  QFDB-DDECIMAL-194           PIC 9(02).
           03  QFDB-DNAME-195              PIC X(08).
           03  QFDB-DOFFSET-195            PIC 9(04).
           03  QFDB-DDIGITS-195            PIC 9(02).
           03  QFDB-DTYPE-195              PIC 9(02).
           03  QFDB-DDECIMAL-195           PIC 9(02).
           03  QFDB-DNAME-196              PIC X(08).
           03  QFDB-DOFFSET-196            PIC 9(04).
           03  QFDB-DDIGITS-196            PIC 9(02).
           03  QFDB-DTYPE-196              PIC 9(02).
           03  QFDB-DDECIMAL-196           PIC 9(02).
           03  QFDB-DNAME-197              PIC X(08).
           03  QFDB-DOFFSET-197            PIC 9(04).
           03  QFDB-DDIGITS-197            PIC 9(02).
           03  QFDB-DTYPE-197              PIC 9(02).
           03  QFDB-DDECIMAL-197           PIC 9(02).
           03  QFDB-DNAME-198              PIC X(08).
           03  QFDB-DOFFSET-198            PIC 9(04).
           03  QFDB-DDIGITS-198            PIC 9(02).
           03  QFDB-DTYPE-198              PIC 9(02).
           03  QFDB-DDECIMAL-198           PIC 9(02).
           03  QFDB-DNAME-199              PIC X(08).
           03  QFDB-DOFFSET-199            PIC 9(04).
           03  QFDB-DDIGITS-199            PIC 9(02).
           03  QFDB-DTYPE-199              PIC 9(02).
           03  QFDB-DDECIMAL-199           PIC 9(02).
           03  QFDB-DNAME-200              PIC X(08).
           03  QFDB-DOFFSET-200            PIC 9(04).
           03  QFDB-DDIGITS-200            PIC 9(02).
           03  QFDB-DTYPE-200              PIC 9(02).
           03  QFDB-DDECIMAL-200           PIC 9(02).
           03  QFDB-CALLID-1                  PIC X(02).
           03  QFDB-KEYLOC-1                  PIC 9(04).
           03  QFDB-CALLID-2                  PIC X(02).
           03  QFDB-KEYLOC-2                  PIC 9(04).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-DBNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING DBNO       (F3-ALL) ".

       01  LEGEND-END-DBNO. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING DBNO       ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-DBNO                PIC 9(02).
           03  END-DBNO                PIC 9(02).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "DBNO                          ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-FDB-DBNO                   PIC 9(02).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOFDBI_DEF.CPY".
       COPY "LIBIO/IOFDBI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOFDBI_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO FDB-PATH-OVERRIDE.

           MOVE BEG-DBNO                   TO QFDB1-WBEG-DBNO.
           MOVE END-DBNO                   TO QFDB1-WEND-DBNO.

           PERFORM OPEN-FDB1-FILE.
           PERFORM START-FDB1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-FDB1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FDB-DBNO < BEG-DBNO )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( FDB-DBNO > END-DBNO )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FDB-DBNO                   TO D01-FDB-DBNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOFDBI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOFDBI-QUEUE-POS  TO Q-POS.
           MOVE IOFDBI-COPIES-POS TO C-POS.
           MOVE IOFDBI-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-DBNO.

           MOVE LEGEND-BEG-DBNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-DBNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DBNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DBNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-DBNO.

      *-----------------------------------------------------------------
       ENTER-END-DBNO.

           MOVE LEGEND-END-DBNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-DBNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DBNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DBNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DBNO.

           IF ( BEG-DBNO > END-DBNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DBNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-DBNO.

           MOVE "SNNN020BEG-DBNO." TO VDU.
           MOVE 0                    TO BEG-DBNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-DBNO." TO VDU.
           MOVE 99                   TO END-DBNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DBNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )     
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "SNNN020BEG-DBNO."    TO VDU.
           MOVE BEG-DBNO              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-DBNO."    TO VDU.
           MOVE END-DBNO              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE FDB1-KEY     TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DBNO                : "    TO R01-PROMPT.
           MOVE BEG-DBNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DBNO                : "    TO R01-PROMPT.
           MOVE END-DBNO                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - FDBFILE" TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOFDBI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOFDBI-SYSDATE.
           DISPLAY IOFDBI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOFDBI-FORM-FIELDS TO WR-BUF.
           MOVE IOFDBI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOFDBI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOFDBI-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOFDBI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD FDB-REC FROM QFDB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-FDB-FIELDS.
           MOVE QFDB-DBNO TO FDB-DBNO.

           IF ( QFDB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO FDB-LTOUCH-DATE
           ELSE
              MOVE QFDB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO FDB-LTOUCH-DATE.

           MOVE QFDB-DNAME-1 TO FDB-DNAME(1).
           MOVE QFDB-DOFFSET-1 TO FDB-DOFFSET(1).
           MOVE QFDB-DDIGITS-1 TO FDB-DDIGITS(1).
           MOVE QFDB-DTYPE-1 TO FDB-DTYPE(1).
           MOVE QFDB-DDECIMAL-1 TO FDB-DDECIMAL(1).
           MOVE QFDB-DNAME-2 TO FDB-DNAME(2).
           MOVE QFDB-DOFFSET-2 TO FDB-DOFFSET(2).
           MOVE QFDB-DDIGITS-2 TO FDB-DDIGITS(2).
           MOVE QFDB-DTYPE-2 TO FDB-DTYPE(2).
           MOVE QFDB-DDECIMAL-2 TO FDB-DDECIMAL(2).
           MOVE QFDB-DNAME-3 TO FDB-DNAME(3).
           MOVE QFDB-DOFFSET-3 TO FDB-DOFFSET(3).
           MOVE QFDB-DDIGITS-3 TO FDB-DDIGITS(3).
           MOVE QFDB-DTYPE-3 TO FDB-DTYPE(3).
           MOVE QFDB-DDECIMAL-3 TO FDB-DDECIMAL(3).
           MOVE QFDB-DNAME-4 TO FDB-DNAME(4).
           MOVE QFDB-DOFFSET-4 TO FDB-DOFFSET(4).
           MOVE QFDB-DDIGITS-4 TO FDB-DDIGITS(4).
           MOVE QFDB-DTYPE-4 TO FDB-DTYPE(4).
           MOVE QFDB-DDECIMAL-4 TO FDB-DDECIMAL(4).
           MOVE QFDB-DNAME-5 TO FDB-DNAME(5).
           MOVE QFDB-DOFFSET-5 TO FDB-DOFFSET(5).
           MOVE QFDB-DDIGITS-5 TO FDB-DDIGITS(5).
           MOVE QFDB-DTYPE-5 TO FDB-DTYPE(5).
           MOVE QFDB-DDECIMAL-5 TO FDB-DDECIMAL(5).
           MOVE QFDB-DNAME-6 TO FDB-DNAME(6).
           MOVE QFDB-DOFFSET-6 TO FDB-DOFFSET(6).
           MOVE QFDB-DDIGITS-6 TO FDB-DDIGITS(6).
           MOVE QFDB-DTYPE-6 TO FDB-DTYPE(6).
           MOVE QFDB-DDECIMAL-6 TO FDB-DDECIMAL(6).
           MOVE QFDB-DNAME-7 TO FDB-DNAME(7).
           MOVE QFDB-DOFFSET-7 TO FDB-DOFFSET(7).
           MOVE QFDB-DDIGITS-7 TO FDB-DDIGITS(7).
           MOVE QFDB-DTYPE-7 TO FDB-DTYPE(7).
           MOVE QFDB-DDECIMAL-7 TO FDB-DDECIMAL(7).
           MOVE QFDB-DNAME-8 TO FDB-DNAME(8).
           MOVE QFDB-DOFFSET-8 TO FDB-DOFFSET(8).
           MOVE QFDB-DDIGITS-8 TO FDB-DDIGITS(8).
           MOVE QFDB-DTYPE-8 TO FDB-DTYPE(8).
           MOVE QFDB-DDECIMAL-8 TO FDB-DDECIMAL(8).
           MOVE QFDB-DNAME-9 TO FDB-DNAME(9).
           MOVE QFDB-DOFFSET-9 TO FDB-DOFFSET(9).
           MOVE QFDB-DDIGITS-9 TO FDB-DDIGITS(9).
           MOVE QFDB-DTYPE-9 TO FDB-DTYPE(9).
           MOVE QFDB-DDECIMAL-9 TO FDB-DDECIMAL(9).
           MOVE QFDB-DNAME-10 TO FDB-DNAME(10).
           MOVE QFDB-DOFFSET-10 TO FDB-DOFFSET(10).
           MOVE QFDB-DDIGITS-10 TO FDB-DDIGITS(10).
           MOVE QFDB-DTYPE-10 TO FDB-DTYPE(10).
           MOVE QFDB-DDECIMAL-10 TO FDB-DDECIMAL(10).
           MOVE QFDB-DNAME-11 TO FDB-DNAME(11).
           MOVE QFDB-DOFFSET-11 TO FDB-DOFFSET(11).
           MOVE QFDB-DDIGITS-11 TO FDB-DDIGITS(11).
           MOVE QFDB-DTYPE-11 TO FDB-DTYPE(11).
           MOVE QFDB-DDECIMAL-11 TO FDB-DDECIMAL(11).
           MOVE QFDB-DNAME-12 TO FDB-DNAME(12).
           MOVE QFDB-DOFFSET-12 TO FDB-DOFFSET(12).
           MOVE QFDB-DDIGITS-12 TO FDB-DDIGITS(12).
           MOVE QFDB-DTYPE-12 TO FDB-DTYPE(12).
           MOVE QFDB-DDECIMAL-12 TO FDB-DDECIMAL(12).
           MOVE QFDB-DNAME-13 TO FDB-DNAME(13).
           MOVE QFDB-DOFFSET-13 TO FDB-DOFFSET(13).
           MOVE QFDB-DDIGITS-13 TO FDB-DDIGITS(13).
           MOVE QFDB-DTYPE-13 TO FDB-DTYPE(13).
           MOVE QFDB-DDECIMAL-13 TO FDB-DDECIMAL(13).
           MOVE QFDB-DNAME-14 TO FDB-DNAME(14).
           MOVE QFDB-DOFFSET-14 TO FDB-DOFFSET(14).
           MOVE QFDB-DDIGITS-14 TO FDB-DDIGITS(14).
           MOVE QFDB-DTYPE-14 TO FDB-DTYPE(14).
           MOVE QFDB-DDECIMAL-14 TO FDB-DDECIMAL(14).
           MOVE QFDB-DNAME-15 TO FDB-DNAME(15).
           MOVE QFDB-DOFFSET-15 TO FDB-DOFFSET(15).
           MOVE QFDB-DDIGITS-15 TO FDB-DDIGITS(15).
           MOVE QFDB-DTYPE-15 TO FDB-DTYPE(15).
           MOVE QFDB-DDECIMAL-15 TO FDB-DDECIMAL(15).
           MOVE QFDB-DNAME-16 TO FDB-DNAME(16).
           MOVE QFDB-DOFFSET-16 TO FDB-DOFFSET(16).
           MOVE QFDB-DDIGITS-16 TO FDB-DDIGITS(16).
           MOVE QFDB-DTYPE-16 TO FDB-DTYPE(16).
           MOVE QFDB-DDECIMAL-16 TO FDB-DDECIMAL(16).
           MOVE QFDB-DNAME-17 TO FDB-DNAME(17).
           MOVE QFDB-DOFFSET-17 TO FDB-DOFFSET(17).
           MOVE QFDB-DDIGITS-17 TO FDB-DDIGITS(17).
           MOVE QFDB-DTYPE-17 TO FDB-DTYPE(17).
           MOVE QFDB-DDECIMAL-17 TO FDB-DDECIMAL(17).
           MOVE QFDB-DNAME-18 TO FDB-DNAME(18).
           MOVE QFDB-DOFFSET-18 TO FDB-DOFFSET(18).
           MOVE QFDB-DDIGITS-18 TO FDB-DDIGITS(18).
           MOVE QFDB-DTYPE-18 TO FDB-DTYPE(18).
           MOVE QFDB-DDECIMAL-18 TO FDB-DDECIMAL(18).
           MOVE QFDB-DNAME-19 TO FDB-DNAME(19).
           MOVE QFDB-DOFFSET-19 TO FDB-DOFFSET(19).
           MOVE QFDB-DDIGITS-19 TO FDB-DDIGITS(19).
           MOVE QFDB-DTYPE-19 TO FDB-DTYPE(19).
           MOVE QFDB-DDECIMAL-19 TO FDB-DDECIMAL(19).
           MOVE QFDB-DNAME-20 TO FDB-DNAME(20).
           MOVE QFDB-DOFFSET-20 TO FDB-DOFFSET(20).
           MOVE QFDB-DDIGITS-20 TO FDB-DDIGITS(20).
           MOVE QFDB-DTYPE-20 TO FDB-DTYPE(20).
           MOVE QFDB-DDECIMAL-20 TO FDB-DDECIMAL(20).
           MOVE QFDB-DNAME-21 TO FDB-DNAME(21).
           MOVE QFDB-DOFFSET-21 TO FDB-DOFFSET(21).
           MOVE QFDB-DDIGITS-21 TO FDB-DDIGITS(21).
           MOVE QFDB-DTYPE-21 TO FDB-DTYPE(21).
           MOVE QFDB-DDECIMAL-21 TO FDB-DDECIMAL(21).
           MOVE QFDB-DNAME-22 TO FDB-DNAME(22).
           MOVE QFDB-DOFFSET-22 TO FDB-DOFFSET(22).
           MOVE QFDB-DDIGITS-22 TO FDB-DDIGITS(22).
           MOVE QFDB-DTYPE-22 TO FDB-DTYPE(22).
           MOVE QFDB-DDECIMAL-22 TO FDB-DDECIMAL(22).
           MOVE QFDB-DNAME-23 TO FDB-DNAME(23).
           MOVE QFDB-DOFFSET-23 TO FDB-DOFFSET(23).
           MOVE QFDB-DDIGITS-23 TO FDB-DDIGITS(23).
           MOVE QFDB-DTYPE-23 TO FDB-DTYPE(23).
           MOVE QFDB-DDECIMAL-23 TO FDB-DDECIMAL(23).
           MOVE QFDB-DNAME-24 TO FDB-DNAME(24).
           MOVE QFDB-DOFFSET-24 TO FDB-DOFFSET(24).
           MOVE QFDB-DDIGITS-24 TO FDB-DDIGITS(24).
           MOVE QFDB-DTYPE-24 TO FDB-DTYPE(24).
           MOVE QFDB-DDECIMAL-24 TO FDB-DDECIMAL(24).
           MOVE QFDB-DNAME-25 TO FDB-DNAME(25).
           MOVE QFDB-DOFFSET-25 TO FDB-DOFFSET(25).
           MOVE QFDB-DDIGITS-25 TO FDB-DDIGITS(25).
           MOVE QFDB-DTYPE-25 TO FDB-DTYPE(25).
           MOVE QFDB-DDECIMAL-25 TO FDB-DDECIMAL(25).
           MOVE QFDB-DNAME-26 TO FDB-DNAME(26).
           MOVE QFDB-DOFFSET-26 TO FDB-DOFFSET(26).
           MOVE QFDB-DDIGITS-26 TO FDB-DDIGITS(26).
           MOVE QFDB-DTYPE-26 TO FDB-DTYPE(26).
           MOVE QFDB-DDECIMAL-26 TO FDB-DDECIMAL(26).
           MOVE QFDB-DNAME-27 TO FDB-DNAME(27).
           MOVE QFDB-DOFFSET-27 TO FDB-DOFFSET(27).
           MOVE QFDB-DDIGITS-27 TO FDB-DDIGITS(27).
           MOVE QFDB-DTYPE-27 TO FDB-DTYPE(27).
           MOVE QFDB-DDECIMAL-27 TO FDB-DDECIMAL(27).
           MOVE QFDB-DNAME-28 TO FDB-DNAME(28).
           MOVE QFDB-DOFFSET-28 TO FDB-DOFFSET(28).
           MOVE QFDB-DDIGITS-28 TO FDB-DDIGITS(28).
           MOVE QFDB-DTYPE-28 TO FDB-DTYPE(28).
           MOVE QFDB-DDECIMAL-28 TO FDB-DDECIMAL(28).
           MOVE QFDB-DNAME-29 TO FDB-DNAME(29).
           MOVE QFDB-DOFFSET-29 TO FDB-DOFFSET(29).
           MOVE QFDB-DDIGITS-29 TO FDB-DDIGITS(29).
           MOVE QFDB-DTYPE-29 TO FDB-DTYPE(29).
           MOVE QFDB-DDECIMAL-29 TO FDB-DDECIMAL(29).
           MOVE QFDB-DNAME-30 TO FDB-DNAME(30).
           MOVE QFDB-DOFFSET-30 TO FDB-DOFFSET(30).
           MOVE QFDB-DDIGITS-30 TO FDB-DDIGITS(30).
           MOVE QFDB-DTYPE-30 TO FDB-DTYPE(30).
           MOVE QFDB-DDECIMAL-30 TO FDB-DDECIMAL(30).
           MOVE QFDB-DNAME-31 TO FDB-DNAME(31).
           MOVE QFDB-DOFFSET-31 TO FDB-DOFFSET(31).
           MOVE QFDB-DDIGITS-31 TO FDB-DDIGITS(31).
           MOVE QFDB-DTYPE-31 TO FDB-DTYPE(31).
           MOVE QFDB-DDECIMAL-31 TO FDB-DDECIMAL(31).
           MOVE QFDB-DNAME-32 TO FDB-DNAME(32).
           MOVE QFDB-DOFFSET-32 TO FDB-DOFFSET(32).
           MOVE QFDB-DDIGITS-32 TO FDB-DDIGITS(32).
           MOVE QFDB-DTYPE-32 TO FDB-DTYPE(32).
           MOVE QFDB-DDECIMAL-32 TO FDB-DDECIMAL(32).
           MOVE QFDB-DNAME-33 TO FDB-DNAME(33).
           MOVE QFDB-DOFFSET-33 TO FDB-DOFFSET(33).
           MOVE QFDB-DDIGITS-33 TO FDB-DDIGITS(33).
           MOVE QFDB-DTYPE-33 TO FDB-DTYPE(33).
           MOVE QFDB-DDECIMAL-33 TO FDB-DDECIMAL(33).
           MOVE QFDB-DNAME-34 TO FDB-DNAME(34).
           MOVE QFDB-DOFFSET-34 TO FDB-DOFFSET(34).
           MOVE QFDB-DDIGITS-34 TO FDB-DDIGITS(34).
           MOVE QFDB-DTYPE-34 TO FDB-DTYPE(34).
           MOVE QFDB-DDECIMAL-34 TO FDB-DDECIMAL(34).
           MOVE QFDB-DNAME-35 TO FDB-DNAME(35).
           MOVE QFDB-DOFFSET-35 TO FDB-DOFFSET(35).
           MOVE QFDB-DDIGITS-35 TO FDB-DDIGITS(35).
           MOVE QFDB-DTYPE-35 TO FDB-DTYPE(35).
           MOVE QFDB-DDECIMAL-35 TO FDB-DDECIMAL(35).
           MOVE QFDB-DNAME-36 TO FDB-DNAME(36).
           MOVE QFDB-DOFFSET-36 TO FDB-DOFFSET(36).
           MOVE QFDB-DDIGITS-36 TO FDB-DDIGITS(36).
           MOVE QFDB-DTYPE-36 TO FDB-DTYPE(36).
           MOVE QFDB-DDECIMAL-36 TO FDB-DDECIMAL(36).
           MOVE QFDB-DNAME-37 TO FDB-DNAME(37).
           MOVE QFDB-DOFFSET-37 TO FDB-DOFFSET(37).
           MOVE QFDB-DDIGITS-37 TO FDB-DDIGITS(37).
           MOVE QFDB-DTYPE-37 TO FDB-DTYPE(37).
           MOVE QFDB-DDECIMAL-37 TO FDB-DDECIMAL(37).
           MOVE QFDB-DNAME-38 TO FDB-DNAME(38).
           MOVE QFDB-DOFFSET-38 TO FDB-DOFFSET(38).
           MOVE QFDB-DDIGITS-38 TO FDB-DDIGITS(38).
           MOVE QFDB-DTYPE-38 TO FDB-DTYPE(38).
           MOVE QFDB-DDECIMAL-38 TO FDB-DDECIMAL(38).
           MOVE QFDB-DNAME-39 TO FDB-DNAME(39).
           MOVE QFDB-DOFFSET-39 TO FDB-DOFFSET(39).
           MOVE QFDB-DDIGITS-39 TO FDB-DDIGITS(39).
           MOVE QFDB-DTYPE-39 TO FDB-DTYPE(39).
           MOVE QFDB-DDECIMAL-39 TO FDB-DDECIMAL(39).
           MOVE QFDB-DNAME-40 TO FDB-DNAME(40).
           MOVE QFDB-DOFFSET-40 TO FDB-DOFFSET(40).
           MOVE QFDB-DDIGITS-40 TO FDB-DDIGITS(40).
           MOVE QFDB-DTYPE-40 TO FDB-DTYPE(40).
           MOVE QFDB-DDECIMAL-40 TO FDB-DDECIMAL(40).
           MOVE QFDB-DNAME-41 TO FDB-DNAME(41).
           MOVE QFDB-DOFFSET-41 TO FDB-DOFFSET(41).
           MOVE QFDB-DDIGITS-41 TO FDB-DDIGITS(41).
           MOVE QFDB-DTYPE-41 TO FDB-DTYPE(41).
           MOVE QFDB-DDECIMAL-41 TO FDB-DDECIMAL(41).
           MOVE QFDB-DNAME-42 TO FDB-DNAME(42).
           MOVE QFDB-DOFFSET-42 TO FDB-DOFFSET(42).
           MOVE QFDB-DDIGITS-42 TO FDB-DDIGITS(42).
           MOVE QFDB-DTYPE-42 TO FDB-DTYPE(42).
           MOVE QFDB-DDECIMAL-42 TO FDB-DDECIMAL(42).
           MOVE QFDB-DNAME-43 TO FDB-DNAME(43).
           MOVE QFDB-DOFFSET-43 TO FDB-DOFFSET(43).
           MOVE QFDB-DDIGITS-43 TO FDB-DDIGITS(43).
           MOVE QFDB-DTYPE-43 TO FDB-DTYPE(43).
           MOVE QFDB-DDECIMAL-43 TO FDB-DDECIMAL(43).
           MOVE QFDB-DNAME-44 TO FDB-DNAME(44).
           MOVE QFDB-DOFFSET-44 TO FDB-DOFFSET(44).
           MOVE QFDB-DDIGITS-44 TO FDB-DDIGITS(44).
           MOVE QFDB-DTYPE-44 TO FDB-DTYPE(44).
           MOVE QFDB-DDECIMAL-44 TO FDB-DDECIMAL(44).
           MOVE QFDB-DNAME-45 TO FDB-DNAME(45).
           MOVE QFDB-DOFFSET-45 TO FDB-DOFFSET(45).
           MOVE QFDB-DDIGITS-45 TO FDB-DDIGITS(45).
           MOVE QFDB-DTYPE-45 TO FDB-DTYPE(45).
           MOVE QFDB-DDECIMAL-45 TO FDB-DDECIMAL(45).
           MOVE QFDB-DNAME-46 TO FDB-DNAME(46).
           MOVE QFDB-DOFFSET-46 TO FDB-DOFFSET(46).
           MOVE QFDB-DDIGITS-46 TO FDB-DDIGITS(46).
           MOVE QFDB-DTYPE-46 TO FDB-DTYPE(46).
           MOVE QFDB-DDECIMAL-46 TO FDB-DDECIMAL(46).
           MOVE QFDB-DNAME-47 TO FDB-DNAME(47).
           MOVE QFDB-DOFFSET-47 TO FDB-DOFFSET(47).
           MOVE QFDB-DDIGITS-47 TO FDB-DDIGITS(47).
           MOVE QFDB-DTYPE-47 TO FDB-DTYPE(47).
           MOVE QFDB-DDECIMAL-47 TO FDB-DDECIMAL(47).
           MOVE QFDB-DNAME-48 TO FDB-DNAME(48).
           MOVE QFDB-DOFFSET-48 TO FDB-DOFFSET(48).
           MOVE QFDB-DDIGITS-48 TO FDB-DDIGITS(48).
           MOVE QFDB-DTYPE-48 TO FDB-DTYPE(48).
           MOVE QFDB-DDECIMAL-48 TO FDB-DDECIMAL(48).
           MOVE QFDB-DNAME-49 TO FDB-DNAME(49).
           MOVE QFDB-DOFFSET-49 TO FDB-DOFFSET(49).
           MOVE QFDB-DDIGITS-49 TO FDB-DDIGITS(49).
           MOVE QFDB-DTYPE-49 TO FDB-DTYPE(49).
           MOVE QFDB-DDECIMAL-49 TO FDB-DDECIMAL(49).
           MOVE QFDB-DNAME-50 TO FDB-DNAME(50).
           MOVE QFDB-DOFFSET-50 TO FDB-DOFFSET(50).
           MOVE QFDB-DDIGITS-50 TO FDB-DDIGITS(50).
           MOVE QFDB-DTYPE-50 TO FDB-DTYPE(50).
           MOVE QFDB-DDECIMAL-50 TO FDB-DDECIMAL(50).
           MOVE QFDB-DNAME-51 TO FDB-DNAME(51).
           MOVE QFDB-DOFFSET-51 TO FDB-DOFFSET(51).
           MOVE QFDB-DDIGITS-51 TO FDB-DDIGITS(51).
           MOVE QFDB-DTYPE-51 TO FDB-DTYPE(51).
           MOVE QFDB-DDECIMAL-51 TO FDB-DDECIMAL(51).
           MOVE QFDB-DNAME-52 TO FDB-DNAME(52).
           MOVE QFDB-DOFFSET-52 TO FDB-DOFFSET(52).
           MOVE QFDB-DDIGITS-52 TO FDB-DDIGITS(52).
           MOVE QFDB-DTYPE-52 TO FDB-DTYPE(52).
           MOVE QFDB-DDECIMAL-52 TO FDB-DDECIMAL(52).
           MOVE QFDB-DNAME-53 TO FDB-DNAME(53).
           MOVE QFDB-DOFFSET-53 TO FDB-DOFFSET(53).
           MOVE QFDB-DDIGITS-53 TO FDB-DDIGITS(53).
           MOVE QFDB-DTYPE-53 TO FDB-DTYPE(53).
           MOVE QFDB-DDECIMAL-53 TO FDB-DDECIMAL(53).
           MOVE QFDB-DNAME-54 TO FDB-DNAME(54).
           MOVE QFDB-DOFFSET-54 TO FDB-DOFFSET(54).
           MOVE QFDB-DDIGITS-54 TO FDB-DDIGITS(54).
           MOVE QFDB-DTYPE-54 TO FDB-DTYPE(54).
           MOVE QFDB-DDECIMAL-54 TO FDB-DDECIMAL(54).
           MOVE QFDB-DNAME-55 TO FDB-DNAME(55).
           MOVE QFDB-DOFFSET-55 TO FDB-DOFFSET(55).
           MOVE QFDB-DDIGITS-55 TO FDB-DDIGITS(55).
           MOVE QFDB-DTYPE-55 TO FDB-DTYPE(55).
           MOVE QFDB-DDECIMAL-55 TO FDB-DDECIMAL(55).
           MOVE QFDB-DNAME-56 TO FDB-DNAME(56).
           MOVE QFDB-DOFFSET-56 TO FDB-DOFFSET(56).
           MOVE QFDB-DDIGITS-56 TO FDB-DDIGITS(56).
           MOVE QFDB-DTYPE-56 TO FDB-DTYPE(56).
           MOVE QFDB-DDECIMAL-56 TO FDB-DDECIMAL(56).
           MOVE QFDB-DNAME-57 TO FDB-DNAME(57).
           MOVE QFDB-DOFFSET-57 TO FDB-DOFFSET(57).
           MOVE QFDB-DDIGITS-57 TO FDB-DDIGITS(57).
           MOVE QFDB-DTYPE-57 TO FDB-DTYPE(57).
           MOVE QFDB-DDECIMAL-57 TO FDB-DDECIMAL(57).
           MOVE QFDB-DNAME-58 TO FDB-DNAME(58).
           MOVE QFDB-DOFFSET-58 TO FDB-DOFFSET(58).
           MOVE QFDB-DDIGITS-58 TO FDB-DDIGITS(58).
           MOVE QFDB-DTYPE-58 TO FDB-DTYPE(58).
           MOVE QFDB-DDECIMAL-58 TO FDB-DDECIMAL(58).
           MOVE QFDB-DNAME-59 TO FDB-DNAME(59).
           MOVE QFDB-DOFFSET-59 TO FDB-DOFFSET(59).
           MOVE QFDB-DDIGITS-59 TO FDB-DDIGITS(59).
           MOVE QFDB-DTYPE-59 TO FDB-DTYPE(59).
           MOVE QFDB-DDECIMAL-59 TO FDB-DDECIMAL(59).
           MOVE QFDB-DNAME-60 TO FDB-DNAME(60).
           MOVE QFDB-DOFFSET-60 TO FDB-DOFFSET(60).
           MOVE QFDB-DDIGITS-60 TO FDB-DDIGITS(60).
           MOVE QFDB-DTYPE-60 TO FDB-DTYPE(60).
           MOVE QFDB-DDECIMAL-60 TO FDB-DDECIMAL(60).
           MOVE QFDB-DNAME-61 TO FDB-DNAME(61).
           MOVE QFDB-DOFFSET-61 TO FDB-DOFFSET(61).
           MOVE QFDB-DDIGITS-61 TO FDB-DDIGITS(61).
           MOVE QFDB-DTYPE-61 TO FDB-DTYPE(61).
           MOVE QFDB-DDECIMAL-61 TO FDB-DDECIMAL(61).
           MOVE QFDB-DNAME-62 TO FDB-DNAME(62).
           MOVE QFDB-DOFFSET-62 TO FDB-DOFFSET(62).
           MOVE QFDB-DDIGITS-62 TO FDB-DDIGITS(62).
           MOVE QFDB-DTYPE-62 TO FDB-DTYPE(62).
           MOVE QFDB-DDECIMAL-62 TO FDB-DDECIMAL(62).
           MOVE QFDB-DNAME-63 TO FDB-DNAME(63).
           MOVE QFDB-DOFFSET-63 TO FDB-DOFFSET(63).
           MOVE QFDB-DDIGITS-63 TO FDB-DDIGITS(63).
           MOVE QFDB-DTYPE-63 TO FDB-DTYPE(63).
           MOVE QFDB-DDECIMAL-63 TO FDB-DDECIMAL(63).
           MOVE QFDB-DNAME-64 TO FDB-DNAME(64).
           MOVE QFDB-DOFFSET-64 TO FDB-DOFFSET(64).
           MOVE QFDB-DDIGITS-64 TO FDB-DDIGITS(64).
           MOVE QFDB-DTYPE-64 TO FDB-DTYPE(64).
           MOVE QFDB-DDECIMAL-64 TO FDB-DDECIMAL(64).
           MOVE QFDB-DNAME-65 TO FDB-DNAME(65).
           MOVE QFDB-DOFFSET-65 TO FDB-DOFFSET(65).
           MOVE QFDB-DDIGITS-65 TO FDB-DDIGITS(65).
           MOVE QFDB-DTYPE-65 TO FDB-DTYPE(65).
           MOVE QFDB-DDECIMAL-65 TO FDB-DDECIMAL(65).
           MOVE QFDB-DNAME-66 TO FDB-DNAME(66).
           MOVE QFDB-DOFFSET-66 TO FDB-DOFFSET(66).
           MOVE QFDB-DDIGITS-66 TO FDB-DDIGITS(66).
           MOVE QFDB-DTYPE-66 TO FDB-DTYPE(66).
           MOVE QFDB-DDECIMAL-66 TO FDB-DDECIMAL(66).
           MOVE QFDB-DNAME-67 TO FDB-DNAME(67).
           MOVE QFDB-DOFFSET-67 TO FDB-DOFFSET(67).
           MOVE QFDB-DDIGITS-67 TO FDB-DDIGITS(67).
           MOVE QFDB-DTYPE-67 TO FDB-DTYPE(67).
           MOVE QFDB-DDECIMAL-67 TO FDB-DDECIMAL(67).
           MOVE QFDB-DNAME-68 TO FDB-DNAME(68).
           MOVE QFDB-DOFFSET-68 TO FDB-DOFFSET(68).
           MOVE QFDB-DDIGITS-68 TO FDB-DDIGITS(68).
           MOVE QFDB-DTYPE-68 TO FDB-DTYPE(68).
           MOVE QFDB-DDECIMAL-68 TO FDB-DDECIMAL(68).
           MOVE QFDB-DNAME-69 TO FDB-DNAME(69).
           MOVE QFDB-DOFFSET-69 TO FDB-DOFFSET(69).
           MOVE QFDB-DDIGITS-69 TO FDB-DDIGITS(69).
           MOVE QFDB-DTYPE-69 TO FDB-DTYPE(69).
           MOVE QFDB-DDECIMAL-69 TO FDB-DDECIMAL(69).
           MOVE QFDB-DNAME-70 TO FDB-DNAME(70).
           MOVE QFDB-DOFFSET-70 TO FDB-DOFFSET(70).
           MOVE QFDB-DDIGITS-70 TO FDB-DDIGITS(70).
           MOVE QFDB-DTYPE-70 TO FDB-DTYPE(70).
           MOVE QFDB-DDECIMAL-70 TO FDB-DDECIMAL(70).
           MOVE QFDB-DNAME-71 TO FDB-DNAME(71).
           MOVE QFDB-DOFFSET-71 TO FDB-DOFFSET(71).
           MOVE QFDB-DDIGITS-71 TO FDB-DDIGITS(71).
           MOVE QFDB-DTYPE-71 TO FDB-DTYPE(71).
           MOVE QFDB-DDECIMAL-71 TO FDB-DDECIMAL(71).
           MOVE QFDB-DNAME-72 TO FDB-DNAME(72).
           MOVE QFDB-DOFFSET-72 TO FDB-DOFFSET(72).
           MOVE QFDB-DDIGITS-72 TO FDB-DDIGITS(72).
           MOVE QFDB-DTYPE-72 TO FDB-DTYPE(72).
           MOVE QFDB-DDECIMAL-72 TO FDB-DDECIMAL(72).
           MOVE QFDB-DNAME-73 TO FDB-DNAME(73).
           MOVE QFDB-DOFFSET-73 TO FDB-DOFFSET(73).
           MOVE QFDB-DDIGITS-73 TO FDB-DDIGITS(73).
           MOVE QFDB-DTYPE-73 TO FDB-DTYPE(73).
           MOVE QFDB-DDECIMAL-73 TO FDB-DDECIMAL(73).
           MOVE QFDB-DNAME-74 TO FDB-DNAME(74).
           MOVE QFDB-DOFFSET-74 TO FDB-DOFFSET(74).
           MOVE QFDB-DDIGITS-74 TO FDB-DDIGITS(74).
           MOVE QFDB-DTYPE-74 TO FDB-DTYPE(74).
           MOVE QFDB-DDECIMAL-74 TO FDB-DDECIMAL(74).
           MOVE QFDB-DNAME-75 TO FDB-DNAME(75).
           MOVE QFDB-DOFFSET-75 TO FDB-DOFFSET(75).
           MOVE QFDB-DDIGITS-75 TO FDB-DDIGITS(75).
           MOVE QFDB-DTYPE-75 TO FDB-DTYPE(75).
           MOVE QFDB-DDECIMAL-75 TO FDB-DDECIMAL(75).
           MOVE QFDB-DNAME-76 TO FDB-DNAME(76).
           MOVE QFDB-DOFFSET-76 TO FDB-DOFFSET(76).
           MOVE QFDB-DDIGITS-76 TO FDB-DDIGITS(76).
           MOVE QFDB-DTYPE-76 TO FDB-DTYPE(76).
           MOVE QFDB-DDECIMAL-76 TO FDB-DDECIMAL(76).
           MOVE QFDB-DNAME-77 TO FDB-DNAME(77).
           MOVE QFDB-DOFFSET-77 TO FDB-DOFFSET(77).
           MOVE QFDB-DDIGITS-77 TO FDB-DDIGITS(77).
           MOVE QFDB-DTYPE-77 TO FDB-DTYPE(77).
           MOVE QFDB-DDECIMAL-77 TO FDB-DDECIMAL(77).
           MOVE QFDB-DNAME-78 TO FDB-DNAME(78).
           MOVE QFDB-DOFFSET-78 TO FDB-DOFFSET(78).
           MOVE QFDB-DDIGITS-78 TO FDB-DDIGITS(78).
           MOVE QFDB-DTYPE-78 TO FDB-DTYPE(78).
           MOVE QFDB-DDECIMAL-78 TO FDB-DDECIMAL(78).
           MOVE QFDB-DNAME-79 TO FDB-DNAME(79).
           MOVE QFDB-DOFFSET-79 TO FDB-DOFFSET(79).
           MOVE QFDB-DDIGITS-79 TO FDB-DDIGITS(79).
           MOVE QFDB-DTYPE-79 TO FDB-DTYPE(79).
           MOVE QFDB-DDECIMAL-79 TO FDB-DDECIMAL(79).
           MOVE QFDB-DNAME-80 TO FDB-DNAME(80).
           MOVE QFDB-DOFFSET-80 TO FDB-DOFFSET(80).
           MOVE QFDB-DDIGITS-80 TO FDB-DDIGITS(80).
           MOVE QFDB-DTYPE-80 TO FDB-DTYPE(80).
           MOVE QFDB-DDECIMAL-80 TO FDB-DDECIMAL(80).
           MOVE QFDB-DNAME-81 TO FDB-DNAME(81).
           MOVE QFDB-DOFFSET-81 TO FDB-DOFFSET(81).
           MOVE QFDB-DDIGITS-81 TO FDB-DDIGITS(81).
           MOVE QFDB-DTYPE-81 TO FDB-DTYPE(81).
           MOVE QFDB-DDECIMAL-81 TO FDB-DDECIMAL(81).
           MOVE QFDB-DNAME-82 TO FDB-DNAME(82).
           MOVE QFDB-DOFFSET-82 TO FDB-DOFFSET(82).
           MOVE QFDB-DDIGITS-82 TO FDB-DDIGITS(82).
           MOVE QFDB-DTYPE-82 TO FDB-DTYPE(82).
           MOVE QFDB-DDECIMAL-82 TO FDB-DDECIMAL(82).
           MOVE QFDB-DNAME-83 TO FDB-DNAME(83).
           MOVE QFDB-DOFFSET-83 TO FDB-DOFFSET(83).
           MOVE QFDB-DDIGITS-83 TO FDB-DDIGITS(83).
           MOVE QFDB-DTYPE-83 TO FDB-DTYPE(83).
           MOVE QFDB-DDECIMAL-83 TO FDB-DDECIMAL(83).
           MOVE QFDB-DNAME-84 TO FDB-DNAME(84).
           MOVE QFDB-DOFFSET-84 TO FDB-DOFFSET(84).
           MOVE QFDB-DDIGITS-84 TO FDB-DDIGITS(84).
           MOVE QFDB-DTYPE-84 TO FDB-DTYPE(84).
           MOVE QFDB-DDECIMAL-84 TO FDB-DDECIMAL(84).
           MOVE QFDB-DNAME-85 TO FDB-DNAME(85).
           MOVE QFDB-DOFFSET-85 TO FDB-DOFFSET(85).
           MOVE QFDB-DDIGITS-85 TO FDB-DDIGITS(85).
           MOVE QFDB-DTYPE-85 TO FDB-DTYPE(85).
           MOVE QFDB-DDECIMAL-85 TO FDB-DDECIMAL(85).
           MOVE QFDB-DNAME-86 TO FDB-DNAME(86).
           MOVE QFDB-DOFFSET-86 TO FDB-DOFFSET(86).
           MOVE QFDB-DDIGITS-86 TO FDB-DDIGITS(86).
           MOVE QFDB-DTYPE-86 TO FDB-DTYPE(86).
           MOVE QFDB-DDECIMAL-86 TO FDB-DDECIMAL(86).
           MOVE QFDB-DNAME-87 TO FDB-DNAME(87).
           MOVE QFDB-DOFFSET-87 TO FDB-DOFFSET(87).
           MOVE QFDB-DDIGITS-87 TO FDB-DDIGITS(87).
           MOVE QFDB-DTYPE-87 TO FDB-DTYPE(87).
           MOVE QFDB-DDECIMAL-87 TO FDB-DDECIMAL(87).
           MOVE QFDB-DNAME-88 TO FDB-DNAME(88).
           MOVE QFDB-DOFFSET-88 TO FDB-DOFFSET(88).
           MOVE QFDB-DDIGITS-88 TO FDB-DDIGITS(88).
           MOVE QFDB-DTYPE-88 TO FDB-DTYPE(88).
           MOVE QFDB-DDECIMAL-88 TO FDB-DDECIMAL(88).
           MOVE QFDB-DNAME-89 TO FDB-DNAME(89).
           MOVE QFDB-DOFFSET-89 TO FDB-DOFFSET(89).
           MOVE QFDB-DDIGITS-89 TO FDB-DDIGITS(89).
           MOVE QFDB-DTYPE-89 TO FDB-DTYPE(89).
           MOVE QFDB-DDECIMAL-89 TO FDB-DDECIMAL(89).
           MOVE QFDB-DNAME-90 TO FDB-DNAME(90).
           MOVE QFDB-DOFFSET-90 TO FDB-DOFFSET(90).
           MOVE QFDB-DDIGITS-90 TO FDB-DDIGITS(90).
           MOVE QFDB-DTYPE-90 TO FDB-DTYPE(90).
           MOVE QFDB-DDECIMAL-90 TO FDB-DDECIMAL(90).
           MOVE QFDB-DNAME-91 TO FDB-DNAME(91).
           MOVE QFDB-DOFFSET-91 TO FDB-DOFFSET(91).
           MOVE QFDB-DDIGITS-91 TO FDB-DDIGITS(91).
           MOVE QFDB-DTYPE-91 TO FDB-DTYPE(91).
           MOVE QFDB-DDECIMAL-91 TO FDB-DDECIMAL(91).
           MOVE QFDB-DNAME-92 TO FDB-DNAME(92).
           MOVE QFDB-DOFFSET-92 TO FDB-DOFFSET(92).
           MOVE QFDB-DDIGITS-92 TO FDB-DDIGITS(92).
           MOVE QFDB-DTYPE-92 TO FDB-DTYPE(92).
           MOVE QFDB-DDECIMAL-92 TO FDB-DDECIMAL(92).
           MOVE QFDB-DNAME-93 TO FDB-DNAME(93).
           MOVE QFDB-DOFFSET-93 TO FDB-DOFFSET(93).
           MOVE QFDB-DDIGITS-93 TO FDB-DDIGITS(93).
           MOVE QFDB-DTYPE-93 TO FDB-DTYPE(93).
           MOVE QFDB-DDECIMAL-93 TO FDB-DDECIMAL(93).
           MOVE QFDB-DNAME-94 TO FDB-DNAME(94).
           MOVE QFDB-DOFFSET-94 TO FDB-DOFFSET(94).
           MOVE QFDB-DDIGITS-94 TO FDB-DDIGITS(94).
           MOVE QFDB-DTYPE-94 TO FDB-DTYPE(94).
           MOVE QFDB-DDECIMAL-94 TO FDB-DDECIMAL(94).
           MOVE QFDB-DNAME-95 TO FDB-DNAME(95).
           MOVE QFDB-DOFFSET-95 TO FDB-DOFFSET(95).
           MOVE QFDB-DDIGITS-95 TO FDB-DDIGITS(95).
           MOVE QFDB-DTYPE-95 TO FDB-DTYPE(95).
           MOVE QFDB-DDECIMAL-95 TO FDB-DDECIMAL(95).
           MOVE QFDB-DNAME-96 TO FDB-DNAME(96).
           MOVE QFDB-DOFFSET-96 TO FDB-DOFFSET(96).
           MOVE QFDB-DDIGITS-96 TO FDB-DDIGITS(96).
           MOVE QFDB-DTYPE-96 TO FDB-DTYPE(96).
           MOVE QFDB-DDECIMAL-96 TO FDB-DDECIMAL(96).
           MOVE QFDB-DNAME-97 TO FDB-DNAME(97).
           MOVE QFDB-DOFFSET-97 TO FDB-DOFFSET(97).
           MOVE QFDB-DDIGITS-97 TO FDB-DDIGITS(97).
           MOVE QFDB-DTYPE-97 TO FDB-DTYPE(97).
           MOVE QFDB-DDECIMAL-97 TO FDB-DDECIMAL(97).
           MOVE QFDB-DNAME-98 TO FDB-DNAME(98).
           MOVE QFDB-DOFFSET-98 TO FDB-DOFFSET(98).
           MOVE QFDB-DDIGITS-98 TO FDB-DDIGITS(98).
           MOVE QFDB-DTYPE-98 TO FDB-DTYPE(98).
           MOVE QFDB-DDECIMAL-98 TO FDB-DDECIMAL(98).
           MOVE QFDB-DNAME-99 TO FDB-DNAME(99).
           MOVE QFDB-DOFFSET-99 TO FDB-DOFFSET(99).
           MOVE QFDB-DDIGITS-99 TO FDB-DDIGITS(99).
           MOVE QFDB-DTYPE-99 TO FDB-DTYPE(99).
           MOVE QFDB-DDECIMAL-99 TO FDB-DDECIMAL(99).
           MOVE QFDB-DNAME-100 TO FDB-DNAME(100).
           MOVE QFDB-DOFFSET-100 TO FDB-DOFFSET(100).
           MOVE QFDB-DDIGITS-100 TO FDB-DDIGITS(100).
           MOVE QFDB-DTYPE-100 TO FDB-DTYPE(100).
           MOVE QFDB-DDECIMAL-100 TO FDB-DDECIMAL(100).
           MOVE QFDB-DNAME-101 TO FDB-DNAME(101).
           MOVE QFDB-DOFFSET-101 TO FDB-DOFFSET(101).
           MOVE QFDB-DDIGITS-101 TO FDB-DDIGITS(101).
           MOVE QFDB-DTYPE-101 TO FDB-DTYPE(101).
           MOVE QFDB-DDECIMAL-101 TO FDB-DDECIMAL(101).
           MOVE QFDB-DNAME-102 TO FDB-DNAME(102).
           MOVE QFDB-DOFFSET-102 TO FDB-DOFFSET(102).
           MOVE QFDB-DDIGITS-102 TO FDB-DDIGITS(102).
           MOVE QFDB-DTYPE-102 TO FDB-DTYPE(102).
           MOVE QFDB-DDECIMAL-102 TO FDB-DDECIMAL(102).
           MOVE QFDB-DNAME-103 TO FDB-DNAME(103).
           MOVE QFDB-DOFFSET-103 TO FDB-DOFFSET(103).
           MOVE QFDB-DDIGITS-103 TO FDB-DDIGITS(103).
           MOVE QFDB-DTYPE-103 TO FDB-DTYPE(103).
           MOVE QFDB-DDECIMAL-103 TO FDB-DDECIMAL(103).
           MOVE QFDB-DNAME-104 TO FDB-DNAME(104).
           MOVE QFDB-DOFFSET-104 TO FDB-DOFFSET(104).
           MOVE QFDB-DDIGITS-104 TO FDB-DDIGITS(104).
           MOVE QFDB-DTYPE-104 TO FDB-DTYPE(104).
           MOVE QFDB-DDECIMAL-104 TO FDB-DDECIMAL(104).
           MOVE QFDB-DNAME-105 TO FDB-DNAME(105).
           MOVE QFDB-DOFFSET-105 TO FDB-DOFFSET(105).
           MOVE QFDB-DDIGITS-105 TO FDB-DDIGITS(105).
           MOVE QFDB-DTYPE-105 TO FDB-DTYPE(105).
           MOVE QFDB-DDECIMAL-105 TO FDB-DDECIMAL(105).
           MOVE QFDB-DNAME-106 TO FDB-DNAME(106).
           MOVE QFDB-DOFFSET-106 TO FDB-DOFFSET(106).
           MOVE QFDB-DDIGITS-106 TO FDB-DDIGITS(106).
           MOVE QFDB-DTYPE-106 TO FDB-DTYPE(106).
           MOVE QFDB-DDECIMAL-106 TO FDB-DDECIMAL(106).
           MOVE QFDB-DNAME-107 TO FDB-DNAME(107).
           MOVE QFDB-DOFFSET-107 TO FDB-DOFFSET(107).
           MOVE QFDB-DDIGITS-107 TO FDB-DDIGITS(107).
           MOVE QFDB-DTYPE-107 TO FDB-DTYPE(107).
           MOVE QFDB-DDECIMAL-107 TO FDB-DDECIMAL(107).
           MOVE QFDB-DNAME-108 TO FDB-DNAME(108).
           MOVE QFDB-DOFFSET-108 TO FDB-DOFFSET(108).
           MOVE QFDB-DDIGITS-108 TO FDB-DDIGITS(108).
           MOVE QFDB-DTYPE-108 TO FDB-DTYPE(108).
           MOVE QFDB-DDECIMAL-108 TO FDB-DDECIMAL(108).
           MOVE QFDB-DNAME-109 TO FDB-DNAME(109).
           MOVE QFDB-DOFFSET-109 TO FDB-DOFFSET(109).
           MOVE QFDB-DDIGITS-109 TO FDB-DDIGITS(109).
           MOVE QFDB-DTYPE-109 TO FDB-DTYPE(109).
           MOVE QFDB-DDECIMAL-109 TO FDB-DDECIMAL(109).
           MOVE QFDB-DNAME-110 TO FDB-DNAME(110).
           MOVE QFDB-DOFFSET-110 TO FDB-DOFFSET(110).
           MOVE QFDB-DDIGITS-110 TO FDB-DDIGITS(110).
           MOVE QFDB-DTYPE-110 TO FDB-DTYPE(110).
           MOVE QFDB-DDECIMAL-110 TO FDB-DDECIMAL(110).
           MOVE QFDB-DNAME-111 TO FDB-DNAME(111).
           MOVE QFDB-DOFFSET-111 TO FDB-DOFFSET(111).
           MOVE QFDB-DDIGITS-111 TO FDB-DDIGITS(111).
           MOVE QFDB-DTYPE-111 TO FDB-DTYPE(111).
           MOVE QFDB-DDECIMAL-111 TO FDB-DDECIMAL(111).
           MOVE QFDB-DNAME-112 TO FDB-DNAME(112).
           MOVE QFDB-DOFFSET-112 TO FDB-DOFFSET(112).
           MOVE QFDB-DDIGITS-112 TO FDB-DDIGITS(112).
           MOVE QFDB-DTYPE-112 TO FDB-DTYPE(112).
           MOVE QFDB-DDECIMAL-112 TO FDB-DDECIMAL(112).
           MOVE QFDB-DNAME-113 TO FDB-DNAME(113).
           MOVE QFDB-DOFFSET-113 TO FDB-DOFFSET(113).
           MOVE QFDB-DDIGITS-113 TO FDB-DDIGITS(113).
           MOVE QFDB-DTYPE-113 TO FDB-DTYPE(113).
           MOVE QFDB-DDECIMAL-113 TO FDB-DDECIMAL(113).
           MOVE QFDB-DNAME-114 TO FDB-DNAME(114).
           MOVE QFDB-DOFFSET-114 TO FDB-DOFFSET(114).
           MOVE QFDB-DDIGITS-114 TO FDB-DDIGITS(114).
           MOVE QFDB-DTYPE-114 TO FDB-DTYPE(114).
           MOVE QFDB-DDECIMAL-114 TO FDB-DDECIMAL(114).
           MOVE QFDB-DNAME-115 TO FDB-DNAME(115).
           MOVE QFDB-DOFFSET-115 TO FDB-DOFFSET(115).
           MOVE QFDB-DDIGITS-115 TO FDB-DDIGITS(115).
           MOVE QFDB-DTYPE-115 TO FDB-DTYPE(115).
           MOVE QFDB-DDECIMAL-115 TO FDB-DDECIMAL(115).
           MOVE QFDB-DNAME-116 TO FDB-DNAME(116).
           MOVE QFDB-DOFFSET-116 TO FDB-DOFFSET(116).
           MOVE QFDB-DDIGITS-116 TO FDB-DDIGITS(116).
           MOVE QFDB-DTYPE-116 TO FDB-DTYPE(116).
           MOVE QFDB-DDECIMAL-116 TO FDB-DDECIMAL(116).
           MOVE QFDB-DNAME-117 TO FDB-DNAME(117).
           MOVE QFDB-DOFFSET-117 TO FDB-DOFFSET(117).
           MOVE QFDB-DDIGITS-117 TO FDB-DDIGITS(117).
           MOVE QFDB-DTYPE-117 TO FDB-DTYPE(117).
           MOVE QFDB-DDECIMAL-117 TO FDB-DDECIMAL(117).
           MOVE QFDB-DNAME-118 TO FDB-DNAME(118).
           MOVE QFDB-DOFFSET-118 TO FDB-DOFFSET(118).
           MOVE QFDB-DDIGITS-118 TO FDB-DDIGITS(118).
           MOVE QFDB-DTYPE-118 TO FDB-DTYPE(118).
           MOVE QFDB-DDECIMAL-118 TO FDB-DDECIMAL(118).
           MOVE QFDB-DNAME-119 TO FDB-DNAME(119).
           MOVE QFDB-DOFFSET-119 TO FDB-DOFFSET(119).
           MOVE QFDB-DDIGITS-119 TO FDB-DDIGITS(119).
           MOVE QFDB-DTYPE-119 TO FDB-DTYPE(119).
           MOVE QFDB-DDECIMAL-119 TO FDB-DDECIMAL(119).
           MOVE QFDB-DNAME-120 TO FDB-DNAME(120).
           MOVE QFDB-DOFFSET-120 TO FDB-DOFFSET(120).
           MOVE QFDB-DDIGITS-120 TO FDB-DDIGITS(120).
           MOVE QFDB-DTYPE-120 TO FDB-DTYPE(120).
           MOVE QFDB-DDECIMAL-120 TO FDB-DDECIMAL(120).
           MOVE QFDB-DNAME-121 TO FDB-DNAME(121).
           MOVE QFDB-DOFFSET-121 TO FDB-DOFFSET(121).
           MOVE QFDB-DDIGITS-121 TO FDB-DDIGITS(121).
           MOVE QFDB-DTYPE-121 TO FDB-DTYPE(121).
           MOVE QFDB-DDECIMAL-121 TO FDB-DDECIMAL(121).
           MOVE QFDB-DNAME-122 TO FDB-DNAME(122).
           MOVE QFDB-DOFFSET-122 TO FDB-DOFFSET(122).
           MOVE QFDB-DDIGITS-122 TO FDB-DDIGITS(122).
           MOVE QFDB-DTYPE-122 TO FDB-DTYPE(122).
           MOVE QFDB-DDECIMAL-122 TO FDB-DDECIMAL(122).
           MOVE QFDB-DNAME-123 TO FDB-DNAME(123).
           MOVE QFDB-DOFFSET-123 TO FDB-DOFFSET(123).
           MOVE QFDB-DDIGITS-123 TO FDB-DDIGITS(123).
           MOVE QFDB-DTYPE-123 TO FDB-DTYPE(123).
           MOVE QFDB-DDECIMAL-123 TO FDB-DDECIMAL(123).
           MOVE QFDB-DNAME-124 TO FDB-DNAME(124).
           MOVE QFDB-DOFFSET-124 TO FDB-DOFFSET(124).
           MOVE QFDB-DDIGITS-124 TO FDB-DDIGITS(124).
           MOVE QFDB-DTYPE-124 TO FDB-DTYPE(124).
           MOVE QFDB-DDECIMAL-124 TO FDB-DDECIMAL(124).
           MOVE QFDB-DNAME-125 TO FDB-DNAME(125).
           MOVE QFDB-DOFFSET-125 TO FDB-DOFFSET(125).
           MOVE QFDB-DDIGITS-125 TO FDB-DDIGITS(125).
           MOVE QFDB-DTYPE-125 TO FDB-DTYPE(125).
           MOVE QFDB-DDECIMAL-125 TO FDB-DDECIMAL(125).
           MOVE QFDB-DNAME-126 TO FDB-DNAME(126).
           MOVE QFDB-DOFFSET-126 TO FDB-DOFFSET(126).
           MOVE QFDB-DDIGITS-126 TO FDB-DDIGITS(126).
           MOVE QFDB-DTYPE-126 TO FDB-DTYPE(126).
           MOVE QFDB-DDECIMAL-126 TO FDB-DDECIMAL(126).
           MOVE QFDB-DNAME-127 TO FDB-DNAME(127).
           MOVE QFDB-DOFFSET-127 TO FDB-DOFFSET(127).
           MOVE QFDB-DDIGITS-127 TO FDB-DDIGITS(127).
           MOVE QFDB-DTYPE-127 TO FDB-DTYPE(127).
           MOVE QFDB-DDECIMAL-127 TO FDB-DDECIMAL(127).
           MOVE QFDB-DNAME-128 TO FDB-DNAME(128).
           MOVE QFDB-DOFFSET-128 TO FDB-DOFFSET(128).
           MOVE QFDB-DDIGITS-128 TO FDB-DDIGITS(128).
           MOVE QFDB-DTYPE-128 TO FDB-DTYPE(128).
           MOVE QFDB-DDECIMAL-128 TO FDB-DDECIMAL(128).
           MOVE QFDB-DNAME-129 TO FDB-DNAME(129).
           MOVE QFDB-DOFFSET-129 TO FDB-DOFFSET(129).
           MOVE QFDB-DDIGITS-129 TO FDB-DDIGITS(129).
           MOVE QFDB-DTYPE-129 TO FDB-DTYPE(129).
           MOVE QFDB-DDECIMAL-129 TO FDB-DDECIMAL(129).
           MOVE QFDB-DNAME-130 TO FDB-DNAME(130).
           MOVE QFDB-DOFFSET-130 TO FDB-DOFFSET(130).
           MOVE QFDB-DDIGITS-130 TO FDB-DDIGITS(130).
           MOVE QFDB-DTYPE-130 TO FDB-DTYPE(130).
           MOVE QFDB-DDECIMAL-130 TO FDB-DDECIMAL(130).
           MOVE QFDB-DNAME-131 TO FDB-DNAME(131).
           MOVE QFDB-DOFFSET-131 TO FDB-DOFFSET(131).
           MOVE QFDB-DDIGITS-131 TO FDB-DDIGITS(131).
           MOVE QFDB-DTYPE-131 TO FDB-DTYPE(131).
           MOVE QFDB-DDECIMAL-131 TO FDB-DDECIMAL(131).
           MOVE QFDB-DNAME-132 TO FDB-DNAME(132).
           MOVE QFDB-DOFFSET-132 TO FDB-DOFFSET(132).
           MOVE QFDB-DDIGITS-132 TO FDB-DDIGITS(132).
           MOVE QFDB-DTYPE-132 TO FDB-DTYPE(132).
           MOVE QFDB-DDECIMAL-132 TO FDB-DDECIMAL(132).
           MOVE QFDB-DNAME-133 TO FDB-DNAME(133).
           MOVE QFDB-DOFFSET-133 TO FDB-DOFFSET(133).
           MOVE QFDB-DDIGITS-133 TO FDB-DDIGITS(133).
           MOVE QFDB-DTYPE-133 TO FDB-DTYPE(133).
           MOVE QFDB-DDECIMAL-133 TO FDB-DDECIMAL(133).
           MOVE QFDB-DNAME-134 TO FDB-DNAME(134).
           MOVE QFDB-DOFFSET-134 TO FDB-DOFFSET(134).
           MOVE QFDB-DDIGITS-134 TO FDB-DDIGITS(134).
           MOVE QFDB-DTYPE-134 TO FDB-DTYPE(134).
           MOVE QFDB-DDECIMAL-134 TO FDB-DDECIMAL(134).
           MOVE QFDB-DNAME-135 TO FDB-DNAME(135).
           MOVE QFDB-DOFFSET-135 TO FDB-DOFFSET(135).
           MOVE QFDB-DDIGITS-135 TO FDB-DDIGITS(135).
           MOVE QFDB-DTYPE-135 TO FDB-DTYPE(135).
           MOVE QFDB-DDECIMAL-135 TO FDB-DDECIMAL(135).
           MOVE QFDB-DNAME-136 TO FDB-DNAME(136).
           MOVE QFDB-DOFFSET-136 TO FDB-DOFFSET(136).
           MOVE QFDB-DDIGITS-136 TO FDB-DDIGITS(136).
           MOVE QFDB-DTYPE-136 TO FDB-DTYPE(136).
           MOVE QFDB-DDECIMAL-136 TO FDB-DDECIMAL(136).
           MOVE QFDB-DNAME-137 TO FDB-DNAME(137).
           MOVE QFDB-DOFFSET-137 TO FDB-DOFFSET(137).
           MOVE QFDB-DDIGITS-137 TO FDB-DDIGITS(137).
           MOVE QFDB-DTYPE-137 TO FDB-DTYPE(137).
           MOVE QFDB-DDECIMAL-137 TO FDB-DDECIMAL(137).
           MOVE QFDB-DNAME-138 TO FDB-DNAME(138).
           MOVE QFDB-DOFFSET-138 TO FDB-DOFFSET(138).
           MOVE QFDB-DDIGITS-138 TO FDB-DDIGITS(138).
           MOVE QFDB-DTYPE-138 TO FDB-DTYPE(138).
           MOVE QFDB-DDECIMAL-138 TO FDB-DDECIMAL(138).
           MOVE QFDB-DNAME-139 TO FDB-DNAME(139).
           MOVE QFDB-DOFFSET-139 TO FDB-DOFFSET(139).
           MOVE QFDB-DDIGITS-139 TO FDB-DDIGITS(139).
           MOVE QFDB-DTYPE-139 TO FDB-DTYPE(139).
           MOVE QFDB-DDECIMAL-139 TO FDB-DDECIMAL(139).
           MOVE QFDB-DNAME-140 TO FDB-DNAME(140).
           MOVE QFDB-DOFFSET-140 TO FDB-DOFFSET(140).
           MOVE QFDB-DDIGITS-140 TO FDB-DDIGITS(140).
           MOVE QFDB-DTYPE-140 TO FDB-DTYPE(140).
           MOVE QFDB-DDECIMAL-140 TO FDB-DDECIMAL(140).
           MOVE QFDB-DNAME-141 TO FDB-DNAME(141).
           MOVE QFDB-DOFFSET-141 TO FDB-DOFFSET(141).
           MOVE QFDB-DDIGITS-141 TO FDB-DDIGITS(141).
           MOVE QFDB-DTYPE-141 TO FDB-DTYPE(141).
           MOVE QFDB-DDECIMAL-141 TO FDB-DDECIMAL(141).
           MOVE QFDB-DNAME-142 TO FDB-DNAME(142).
           MOVE QFDB-DOFFSET-142 TO FDB-DOFFSET(142).
           MOVE QFDB-DDIGITS-142 TO FDB-DDIGITS(142).
           MOVE QFDB-DTYPE-142 TO FDB-DTYPE(142).
           MOVE QFDB-DDECIMAL-142 TO FDB-DDECIMAL(142).
           MOVE QFDB-DNAME-143 TO FDB-DNAME(143).
           MOVE QFDB-DOFFSET-143 TO FDB-DOFFSET(143).
           MOVE QFDB-DDIGITS-143 TO FDB-DDIGITS(143).
           MOVE QFDB-DTYPE-143 TO FDB-DTYPE(143).
           MOVE QFDB-DDECIMAL-143 TO FDB-DDECIMAL(143).
           MOVE QFDB-DNAME-144 TO FDB-DNAME(144).
           MOVE QFDB-DOFFSET-144 TO FDB-DOFFSET(144).
           MOVE QFDB-DDIGITS-144 TO FDB-DDIGITS(144).
           MOVE QFDB-DTYPE-144 TO FDB-DTYPE(144).
           MOVE QFDB-DDECIMAL-144 TO FDB-DDECIMAL(144).
           MOVE QFDB-DNAME-145 TO FDB-DNAME(145).
           MOVE QFDB-DOFFSET-145 TO FDB-DOFFSET(145).
           MOVE QFDB-DDIGITS-145 TO FDB-DDIGITS(145).
           MOVE QFDB-DTYPE-145 TO FDB-DTYPE(145).
           MOVE QFDB-DDECIMAL-145 TO FDB-DDECIMAL(145).
           MOVE QFDB-DNAME-146 TO FDB-DNAME(146).
           MOVE QFDB-DOFFSET-146 TO FDB-DOFFSET(146).
           MOVE QFDB-DDIGITS-146 TO FDB-DDIGITS(146).
           MOVE QFDB-DTYPE-146 TO FDB-DTYPE(146).
           MOVE QFDB-DDECIMAL-146 TO FDB-DDECIMAL(146).
           MOVE QFDB-DNAME-147 TO FDB-DNAME(147).
           MOVE QFDB-DOFFSET-147 TO FDB-DOFFSET(147).
           MOVE QFDB-DDIGITS-147 TO FDB-DDIGITS(147).
           MOVE QFDB-DTYPE-147 TO FDB-DTYPE(147).
           MOVE QFDB-DDECIMAL-147 TO FDB-DDECIMAL(147).
           MOVE QFDB-DNAME-148 TO FDB-DNAME(148).
           MOVE QFDB-DOFFSET-148 TO FDB-DOFFSET(148).
           MOVE QFDB-DDIGITS-148 TO FDB-DDIGITS(148).
           MOVE QFDB-DTYPE-148 TO FDB-DTYPE(148).
           MOVE QFDB-DDECIMAL-148 TO FDB-DDECIMAL(148).
           MOVE QFDB-DNAME-149 TO FDB-DNAME(149).
           MOVE QFDB-DOFFSET-149 TO FDB-DOFFSET(149).
           MOVE QFDB-DDIGITS-149 TO FDB-DDIGITS(149).
           MOVE QFDB-DTYPE-149 TO FDB-DTYPE(149).
           MOVE QFDB-DDECIMAL-149 TO FDB-DDECIMAL(149).
           MOVE QFDB-DNAME-150 TO FDB-DNAME(150).
           MOVE QFDB-DOFFSET-150 TO FDB-DOFFSET(150).
           MOVE QFDB-DDIGITS-150 TO FDB-DDIGITS(150).
           MOVE QFDB-DTYPE-150 TO FDB-DTYPE(150).
           MOVE QFDB-DDECIMAL-150 TO FDB-DDECIMAL(150).
           MOVE QFDB-DNAME-151 TO FDB-DNAME(151).
           MOVE QFDB-DOFFSET-151 TO FDB-DOFFSET(151).
           MOVE QFDB-DDIGITS-151 TO FDB-DDIGITS(151).
           MOVE QFDB-DTYPE-151 TO FDB-DTYPE(151).
           MOVE QFDB-DDECIMAL-151 TO FDB-DDECIMAL(151).
           MOVE QFDB-DNAME-152 TO FDB-DNAME(152).
           MOVE QFDB-DOFFSET-152 TO FDB-DOFFSET(152).
           MOVE QFDB-DDIGITS-152 TO FDB-DDIGITS(152).
           MOVE QFDB-DTYPE-152 TO FDB-DTYPE(152).
           MOVE QFDB-DDECIMAL-152 TO FDB-DDECIMAL(152).
           MOVE QFDB-DNAME-153 TO FDB-DNAME(153).
           MOVE QFDB-DOFFSET-153 TO FDB-DOFFSET(153).
           MOVE QFDB-DDIGITS-153 TO FDB-DDIGITS(153).
           MOVE QFDB-DTYPE-153 TO FDB-DTYPE(153).
           MOVE QFDB-DDECIMAL-153 TO FDB-DDECIMAL(153).
           MOVE QFDB-DNAME-154 TO FDB-DNAME(154).
           MOVE QFDB-DOFFSET-154 TO FDB-DOFFSET(154).
           MOVE QFDB-DDIGITS-154 TO FDB-DDIGITS(154).
           MOVE QFDB-DTYPE-154 TO FDB-DTYPE(154).
           MOVE QFDB-DDECIMAL-154 TO FDB-DDECIMAL(154).
           MOVE QFDB-DNAME-155 TO FDB-DNAME(155).
           MOVE QFDB-DOFFSET-155 TO FDB-DOFFSET(155).
           MOVE QFDB-DDIGITS-155 TO FDB-DDIGITS(155).
           MOVE QFDB-DTYPE-155 TO FDB-DTYPE(155).
           MOVE QFDB-DDECIMAL-155 TO FDB-DDECIMAL(155).
           MOVE QFDB-DNAME-156 TO FDB-DNAME(156).
           MOVE QFDB-DOFFSET-156 TO FDB-DOFFSET(156).
           MOVE QFDB-DDIGITS-156 TO FDB-DDIGITS(156).
           MOVE QFDB-DTYPE-156 TO FDB-DTYPE(156).
           MOVE QFDB-DDECIMAL-156 TO FDB-DDECIMAL(156).
           MOVE QFDB-DNAME-157 TO FDB-DNAME(157).
           MOVE QFDB-DOFFSET-157 TO FDB-DOFFSET(157).
           MOVE QFDB-DDIGITS-157 TO FDB-DDIGITS(157).
           MOVE QFDB-DTYPE-157 TO FDB-DTYPE(157).
           MOVE QFDB-DDECIMAL-157 TO FDB-DDECIMAL(157).
           MOVE QFDB-DNAME-158 TO FDB-DNAME(158).
           MOVE QFDB-DOFFSET-158 TO FDB-DOFFSET(158).
           MOVE QFDB-DDIGITS-158 TO FDB-DDIGITS(158).
           MOVE QFDB-DTYPE-158 TO FDB-DTYPE(158).
           MOVE QFDB-DDECIMAL-158 TO FDB-DDECIMAL(158).
           MOVE QFDB-DNAME-159 TO FDB-DNAME(159).
           MOVE QFDB-DOFFSET-159 TO FDB-DOFFSET(159).
           MOVE QFDB-DDIGITS-159 TO FDB-DDIGITS(159).
           MOVE QFDB-DTYPE-159 TO FDB-DTYPE(159).
           MOVE QFDB-DDECIMAL-159 TO FDB-DDECIMAL(159).
           MOVE QFDB-DNAME-160 TO FDB-DNAME(160).
           MOVE QFDB-DOFFSET-160 TO FDB-DOFFSET(160).
           MOVE QFDB-DDIGITS-160 TO FDB-DDIGITS(160).
           MOVE QFDB-DTYPE-160 TO FDB-DTYPE(160).
           MOVE QFDB-DDECIMAL-160 TO FDB-DDECIMAL(160).
           MOVE QFDB-DNAME-161 TO FDB-DNAME(161).
           MOVE QFDB-DOFFSET-161 TO FDB-DOFFSET(161).
           MOVE QFDB-DDIGITS-161 TO FDB-DDIGITS(161).
           MOVE QFDB-DTYPE-161 TO FDB-DTYPE(161).
           MOVE QFDB-DDECIMAL-161 TO FDB-DDECIMAL(161).
           MOVE QFDB-DNAME-162 TO FDB-DNAME(162).
           MOVE QFDB-DOFFSET-162 TO FDB-DOFFSET(162).
           MOVE QFDB-DDIGITS-162 TO FDB-DDIGITS(162).
           MOVE QFDB-DTYPE-162 TO FDB-DTYPE(162).
           MOVE QFDB-DDECIMAL-162 TO FDB-DDECIMAL(162).
           MOVE QFDB-DNAME-163 TO FDB-DNAME(163).
           MOVE QFDB-DOFFSET-163 TO FDB-DOFFSET(163).
           MOVE QFDB-DDIGITS-163 TO FDB-DDIGITS(163).
           MOVE QFDB-DTYPE-163 TO FDB-DTYPE(163).
           MOVE QFDB-DDECIMAL-163 TO FDB-DDECIMAL(163).
           MOVE QFDB-DNAME-164 TO FDB-DNAME(164).
           MOVE QFDB-DOFFSET-164 TO FDB-DOFFSET(164).
           MOVE QFDB-DDIGITS-164 TO FDB-DDIGITS(164).
           MOVE QFDB-DTYPE-164 TO FDB-DTYPE(164).
           MOVE QFDB-DDECIMAL-164 TO FDB-DDECIMAL(164).
           MOVE QFDB-DNAME-165 TO FDB-DNAME(165).
           MOVE QFDB-DOFFSET-165 TO FDB-DOFFSET(165).
           MOVE QFDB-DDIGITS-165 TO FDB-DDIGITS(165).
           MOVE QFDB-DTYPE-165 TO FDB-DTYPE(165).
           MOVE QFDB-DDECIMAL-165 TO FDB-DDECIMAL(165).
           MOVE QFDB-DNAME-166 TO FDB-DNAME(166).
           MOVE QFDB-DOFFSET-166 TO FDB-DOFFSET(166).
           MOVE QFDB-DDIGITS-166 TO FDB-DDIGITS(166).
           MOVE QFDB-DTYPE-166 TO FDB-DTYPE(166).
           MOVE QFDB-DDECIMAL-166 TO FDB-DDECIMAL(166).
           MOVE QFDB-DNAME-167 TO FDB-DNAME(167).
           MOVE QFDB-DOFFSET-167 TO FDB-DOFFSET(167).
           MOVE QFDB-DDIGITS-167 TO FDB-DDIGITS(167).
           MOVE QFDB-DTYPE-167 TO FDB-DTYPE(167).
           MOVE QFDB-DDECIMAL-167 TO FDB-DDECIMAL(167).
           MOVE QFDB-DNAME-168 TO FDB-DNAME(168).
           MOVE QFDB-DOFFSET-168 TO FDB-DOFFSET(168).
           MOVE QFDB-DDIGITS-168 TO FDB-DDIGITS(168).
           MOVE QFDB-DTYPE-168 TO FDB-DTYPE(168).
           MOVE QFDB-DDECIMAL-168 TO FDB-DDECIMAL(168).
           MOVE QFDB-DNAME-169 TO FDB-DNAME(169).
           MOVE QFDB-DOFFSET-169 TO FDB-DOFFSET(169).
           MOVE QFDB-DDIGITS-169 TO FDB-DDIGITS(169).
           MOVE QFDB-DTYPE-169 TO FDB-DTYPE(169).
           MOVE QFDB-DDECIMAL-169 TO FDB-DDECIMAL(169).
           MOVE QFDB-DNAME-170 TO FDB-DNAME(170).
           MOVE QFDB-DOFFSET-170 TO FDB-DOFFSET(170).
           MOVE QFDB-DDIGITS-170 TO FDB-DDIGITS(170).
           MOVE QFDB-DTYPE-170 TO FDB-DTYPE(170).
           MOVE QFDB-DDECIMAL-170 TO FDB-DDECIMAL(170).
           MOVE QFDB-DNAME-171 TO FDB-DNAME(171).
           MOVE QFDB-DOFFSET-171 TO FDB-DOFFSET(171).
           MOVE QFDB-DDIGITS-171 TO FDB-DDIGITS(171).
           MOVE QFDB-DTYPE-171 TO FDB-DTYPE(171).
           MOVE QFDB-DDECIMAL-171 TO FDB-DDECIMAL(171).
           MOVE QFDB-DNAME-172 TO FDB-DNAME(172).
           MOVE QFDB-DOFFSET-172 TO FDB-DOFFSET(172).
           MOVE QFDB-DDIGITS-172 TO FDB-DDIGITS(172).
           MOVE QFDB-DTYPE-172 TO FDB-DTYPE(172).
           MOVE QFDB-DDECIMAL-172 TO FDB-DDECIMAL(172).
           MOVE QFDB-DNAME-173 TO FDB-DNAME(173).
           MOVE QFDB-DOFFSET-173 TO FDB-DOFFSET(173).
           MOVE QFDB-DDIGITS-173 TO FDB-DDIGITS(173).
           MOVE QFDB-DTYPE-173 TO FDB-DTYPE(173).
           MOVE QFDB-DDECIMAL-173 TO FDB-DDECIMAL(173).
           MOVE QFDB-DNAME-174 TO FDB-DNAME(174).
           MOVE QFDB-DOFFSET-174 TO FDB-DOFFSET(174).
           MOVE QFDB-DDIGITS-174 TO FDB-DDIGITS(174).
           MOVE QFDB-DTYPE-174 TO FDB-DTYPE(174).
           MOVE QFDB-DDECIMAL-174 TO FDB-DDECIMAL(174).
           MOVE QFDB-DNAME-175 TO FDB-DNAME(175).
           MOVE QFDB-DOFFSET-175 TO FDB-DOFFSET(175).
           MOVE QFDB-DDIGITS-175 TO FDB-DDIGITS(175).
           MOVE QFDB-DTYPE-175 TO FDB-DTYPE(175).
           MOVE QFDB-DDECIMAL-175 TO FDB-DDECIMAL(175).
           MOVE QFDB-DNAME-176 TO FDB-DNAME(176).
           MOVE QFDB-DOFFSET-176 TO FDB-DOFFSET(176).
           MOVE QFDB-DDIGITS-176 TO FDB-DDIGITS(176).
           MOVE QFDB-DTYPE-176 TO FDB-DTYPE(176).
           MOVE QFDB-DDECIMAL-176 TO FDB-DDECIMAL(176).
           MOVE QFDB-DNAME-177 TO FDB-DNAME(177).
           MOVE QFDB-DOFFSET-177 TO FDB-DOFFSET(177).
           MOVE QFDB-DDIGITS-177 TO FDB-DDIGITS(177).
           MOVE QFDB-DTYPE-177 TO FDB-DTYPE(177).
           MOVE QFDB-DDECIMAL-177 TO FDB-DDECIMAL(177).
           MOVE QFDB-DNAME-178 TO FDB-DNAME(178).
           MOVE QFDB-DOFFSET-178 TO FDB-DOFFSET(178).
           MOVE QFDB-DDIGITS-178 TO FDB-DDIGITS(178).
           MOVE QFDB-DTYPE-178 TO FDB-DTYPE(178).
           MOVE QFDB-DDECIMAL-178 TO FDB-DDECIMAL(178).
           MOVE QFDB-DNAME-179 TO FDB-DNAME(179).
           MOVE QFDB-DOFFSET-179 TO FDB-DOFFSET(179).
           MOVE QFDB-DDIGITS-179 TO FDB-DDIGITS(179).
           MOVE QFDB-DTYPE-179 TO FDB-DTYPE(179).
           MOVE QFDB-DDECIMAL-179 TO FDB-DDECIMAL(179).
           MOVE QFDB-DNAME-180 TO FDB-DNAME(180).
           MOVE QFDB-DOFFSET-180 TO FDB-DOFFSET(180).
           MOVE QFDB-DDIGITS-180 TO FDB-DDIGITS(180).
           MOVE QFDB-DTYPE-180 TO FDB-DTYPE(180).
           MOVE QFDB-DDECIMAL-180 TO FDB-DDECIMAL(180).
           MOVE QFDB-DNAME-181 TO FDB-DNAME(181).
           MOVE QFDB-DOFFSET-181 TO FDB-DOFFSET(181).
           MOVE QFDB-DDIGITS-181 TO FDB-DDIGITS(181).
           MOVE QFDB-DTYPE-181 TO FDB-DTYPE(181).
           MOVE QFDB-DDECIMAL-181 TO FDB-DDECIMAL(181).
           MOVE QFDB-DNAME-182 TO FDB-DNAME(182).
           MOVE QFDB-DOFFSET-182 TO FDB-DOFFSET(182).
           MOVE QFDB-DDIGITS-182 TO FDB-DDIGITS(182).
           MOVE QFDB-DTYPE-182 TO FDB-DTYPE(182).
           MOVE QFDB-DDECIMAL-182 TO FDB-DDECIMAL(182).
           MOVE QFDB-DNAME-183 TO FDB-DNAME(183).
           MOVE QFDB-DOFFSET-183 TO FDB-DOFFSET(183).
           MOVE QFDB-DDIGITS-183 TO FDB-DDIGITS(183).
           MOVE QFDB-DTYPE-183 TO FDB-DTYPE(183).
           MOVE QFDB-DDECIMAL-183 TO FDB-DDECIMAL(183).
           MOVE QFDB-DNAME-184 TO FDB-DNAME(184).
           MOVE QFDB-DOFFSET-184 TO FDB-DOFFSET(184).
           MOVE QFDB-DDIGITS-184 TO FDB-DDIGITS(184).
           MOVE QFDB-DTYPE-184 TO FDB-DTYPE(184).
           MOVE QFDB-DDECIMAL-184 TO FDB-DDECIMAL(184).
           MOVE QFDB-DNAME-185 TO FDB-DNAME(185).
           MOVE QFDB-DOFFSET-185 TO FDB-DOFFSET(185).
           MOVE QFDB-DDIGITS-185 TO FDB-DDIGITS(185).
           MOVE QFDB-DTYPE-185 TO FDB-DTYPE(185).
           MOVE QFDB-DDECIMAL-185 TO FDB-DDECIMAL(185).
           MOVE QFDB-DNAME-186 TO FDB-DNAME(186).
           MOVE QFDB-DOFFSET-186 TO FDB-DOFFSET(186).
           MOVE QFDB-DDIGITS-186 TO FDB-DDIGITS(186).
           MOVE QFDB-DTYPE-186 TO FDB-DTYPE(186).
           MOVE QFDB-DDECIMAL-186 TO FDB-DDECIMAL(186).
           MOVE QFDB-DNAME-187 TO FDB-DNAME(187).
           MOVE QFDB-DOFFSET-187 TO FDB-DOFFSET(187).
           MOVE QFDB-DDIGITS-187 TO FDB-DDIGITS(187).
           MOVE QFDB-DTYPE-187 TO FDB-DTYPE(187).
           MOVE QFDB-DDECIMAL-187 TO FDB-DDECIMAL(187).
           MOVE QFDB-DNAME-188 TO FDB-DNAME(188).
           MOVE QFDB-DOFFSET-188 TO FDB-DOFFSET(188).
           MOVE QFDB-DDIGITS-188 TO FDB-DDIGITS(188).
           MOVE QFDB-DTYPE-188 TO FDB-DTYPE(188).
           MOVE QFDB-DDECIMAL-188 TO FDB-DDECIMAL(188).
           MOVE QFDB-DNAME-189 TO FDB-DNAME(189).
           MOVE QFDB-DOFFSET-189 TO FDB-DOFFSET(189).
           MOVE QFDB-DDIGITS-189 TO FDB-DDIGITS(189).
           MOVE QFDB-DTYPE-189 TO FDB-DTYPE(189).
           MOVE QFDB-DDECIMAL-189 TO FDB-DDECIMAL(189).
           MOVE QFDB-DNAME-190 TO FDB-DNAME(190).
           MOVE QFDB-DOFFSET-190 TO FDB-DOFFSET(190).
           MOVE QFDB-DDIGITS-190 TO FDB-DDIGITS(190).
           MOVE QFDB-DTYPE-190 TO FDB-DTYPE(190).
           MOVE QFDB-DDECIMAL-190 TO FDB-DDECIMAL(190).
           MOVE QFDB-DNAME-191 TO FDB-DNAME(191).
           MOVE QFDB-DOFFSET-191 TO FDB-DOFFSET(191).
           MOVE QFDB-DDIGITS-191 TO FDB-DDIGITS(191).
           MOVE QFDB-DTYPE-191 TO FDB-DTYPE(191).
           MOVE QFDB-DDECIMAL-191 TO FDB-DDECIMAL(191).
           MOVE QFDB-DNAME-192 TO FDB-DNAME(192).
           MOVE QFDB-DOFFSET-192 TO FDB-DOFFSET(192).
           MOVE QFDB-DDIGITS-192 TO FDB-DDIGITS(192).
           MOVE QFDB-DTYPE-192 TO FDB-DTYPE(192).
           MOVE QFDB-DDECIMAL-192 TO FDB-DDECIMAL(192).
           MOVE QFDB-DNAME-193 TO FDB-DNAME(193).
           MOVE QFDB-DOFFSET-193 TO FDB-DOFFSET(193).
           MOVE QFDB-DDIGITS-193 TO FDB-DDIGITS(193).
           MOVE QFDB-DTYPE-193 TO FDB-DTYPE(193).
           MOVE QFDB-DDECIMAL-193 TO FDB-DDECIMAL(193).
           MOVE QFDB-DNAME-194 TO FDB-DNAME(194).
           MOVE QFDB-DOFFSET-194 TO FDB-DOFFSET(194).
           MOVE QFDB-DDIGITS-194 TO FDB-DDIGITS(194).
           MOVE QFDB-DTYPE-194 TO FDB-DTYPE(194).
           MOVE QFDB-DDECIMAL-194 TO FDB-DDECIMAL(194).
           MOVE QFDB-DNAME-195 TO FDB-DNAME(195).
           MOVE QFDB-DOFFSET-195 TO FDB-DOFFSET(195).
           MOVE QFDB-DDIGITS-195 TO FDB-DDIGITS(195).
           MOVE QFDB-DTYPE-195 TO FDB-DTYPE(195).
           MOVE QFDB-DDECIMAL-195 TO FDB-DDECIMAL(195).
           MOVE QFDB-DNAME-196 TO FDB-DNAME(196).
           MOVE QFDB-DOFFSET-196 TO FDB-DOFFSET(196).
           MOVE QFDB-DDIGITS-196 TO FDB-DDIGITS(196).
           MOVE QFDB-DTYPE-196 TO FDB-DTYPE(196).
           MOVE QFDB-DDECIMAL-196 TO FDB-DDECIMAL(196).
           MOVE QFDB-DNAME-197 TO FDB-DNAME(197).
           MOVE QFDB-DOFFSET-197 TO FDB-DOFFSET(197).
           MOVE QFDB-DDIGITS-197 TO FDB-DDIGITS(197).
           MOVE QFDB-DTYPE-197 TO FDB-DTYPE(197).
           MOVE QFDB-DDECIMAL-197 TO FDB-DDECIMAL(197).
           MOVE QFDB-DNAME-198 TO FDB-DNAME(198).
           MOVE QFDB-DOFFSET-198 TO FDB-DOFFSET(198).
           MOVE QFDB-DDIGITS-198 TO FDB-DDIGITS(198).
           MOVE QFDB-DTYPE-198 TO FDB-DTYPE(198).
           MOVE QFDB-DDECIMAL-198 TO FDB-DDECIMAL(198).
           MOVE QFDB-DNAME-199 TO FDB-DNAME(199).
           MOVE QFDB-DOFFSET-199 TO FDB-DOFFSET(199).
           MOVE QFDB-DDIGITS-199 TO FDB-DDIGITS(199).
           MOVE QFDB-DTYPE-199 TO FDB-DTYPE(199).
           MOVE QFDB-DDECIMAL-199 TO FDB-DDECIMAL(199).
           MOVE QFDB-DNAME-200 TO FDB-DNAME(200).
           MOVE QFDB-DOFFSET-200 TO FDB-DOFFSET(200).
           MOVE QFDB-DDIGITS-200 TO FDB-DDIGITS(200).
           MOVE QFDB-DTYPE-200 TO FDB-DTYPE(200).
           MOVE QFDB-DDECIMAL-200 TO FDB-DDECIMAL(200).
           MOVE QFDB-CALLID-1 TO FDB-CALLID(1).
           MOVE QFDB-KEYLOC-1 TO FDB-KEYLOC(1).
           MOVE QFDB-CALLID-2 TO FDB-CALLID(2).
           MOVE QFDB-KEYLOC-2 TO FDB-KEYLOC(2).
 
      *-----------------------------------------------------------------
      *    LOAD QFDB-REC FROM FDB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-FDB-FIELDS.
           MOVE FDB-DBNO TO QFDB-DBNO.

           IF ( FDB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QFDB-LTOUCH-DATE
           ELSE
              MOVE FDB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QFDB-LTOUCH-DATE.

           MOVE FDB-DNAME(1) TO QFDB-DNAME-1.
           MOVE FDB-DOFFSET(1) TO QFDB-DOFFSET-1.
           MOVE FDB-DDIGITS(1) TO QFDB-DDIGITS-1.
           MOVE FDB-DTYPE(1) TO QFDB-DTYPE-1.
           MOVE FDB-DDECIMAL(1) TO QFDB-DDECIMAL-1.
           MOVE FDB-DNAME(2) TO QFDB-DNAME-2.
           MOVE FDB-DOFFSET(2) TO QFDB-DOFFSET-2.
           MOVE FDB-DDIGITS(2) TO QFDB-DDIGITS-2.
           MOVE FDB-DTYPE(2) TO QFDB-DTYPE-2.
           MOVE FDB-DDECIMAL(2) TO QFDB-DDECIMAL-2.
           MOVE FDB-DNAME(3) TO QFDB-DNAME-3.
           MOVE FDB-DOFFSET(3) TO QFDB-DOFFSET-3.
           MOVE FDB-DDIGITS(3) TO QFDB-DDIGITS-3.
           MOVE FDB-DTYPE(3) TO QFDB-DTYPE-3.
           MOVE FDB-DDECIMAL(3) TO QFDB-DDECIMAL-3.
           MOVE FDB-DNAME(4) TO QFDB-DNAME-4.
           MOVE FDB-DOFFSET(4) TO QFDB-DOFFSET-4.
           MOVE FDB-DDIGITS(4) TO QFDB-DDIGITS-4.
           MOVE FDB-DTYPE(4) TO QFDB-DTYPE-4.
           MOVE FDB-DDECIMAL(4) TO QFDB-DDECIMAL-4.
           MOVE FDB-DNAME(5) TO QFDB-DNAME-5.
           MOVE FDB-DOFFSET(5) TO QFDB-DOFFSET-5.
           MOVE FDB-DDIGITS(5) TO QFDB-DDIGITS-5.
           MOVE FDB-DTYPE(5) TO QFDB-DTYPE-5.
           MOVE FDB-DDECIMAL(5) TO QFDB-DDECIMAL-5.
           MOVE FDB-DNAME(6) TO QFDB-DNAME-6.
           MOVE FDB-DOFFSET(6) TO QFDB-DOFFSET-6.
           MOVE FDB-DDIGITS(6) TO QFDB-DDIGITS-6.
           MOVE FDB-DTYPE(6) TO QFDB-DTYPE-6.
           MOVE FDB-DDECIMAL(6) TO QFDB-DDECIMAL-6.
           MOVE FDB-DNAME(7) TO QFDB-DNAME-7.
           MOVE FDB-DOFFSET(7) TO QFDB-DOFFSET-7.
           MOVE FDB-DDIGITS(7) TO QFDB-DDIGITS-7.
           MOVE FDB-DTYPE(7) TO QFDB-DTYPE-7.
           MOVE FDB-DDECIMAL(7) TO QFDB-DDECIMAL-7.
           MOVE FDB-DNAME(8) TO QFDB-DNAME-8.
           MOVE FDB-DOFFSET(8) TO QFDB-DOFFSET-8.
           MOVE FDB-DDIGITS(8) TO QFDB-DDIGITS-8.
           MOVE FDB-DTYPE(8) TO QFDB-DTYPE-8.
           MOVE FDB-DDECIMAL(8) TO QFDB-DDECIMAL-8.
           MOVE FDB-DNAME(9) TO QFDB-DNAME-9.
           MOVE FDB-DOFFSET(9) TO QFDB-DOFFSET-9.
           MOVE FDB-DDIGITS(9) TO QFDB-DDIGITS-9.
           MOVE FDB-DTYPE(9) TO QFDB-DTYPE-9.
           MOVE FDB-DDECIMAL(9) TO QFDB-DDECIMAL-9.
           MOVE FDB-DNAME(10) TO QFDB-DNAME-10.
           MOVE FDB-DOFFSET(10) TO QFDB-DOFFSET-10.
           MOVE FDB-DDIGITS(10) TO QFDB-DDIGITS-10.
           MOVE FDB-DTYPE(10) TO QFDB-DTYPE-10.
           MOVE FDB-DDECIMAL(10) TO QFDB-DDECIMAL-10.
           MOVE FDB-DNAME(11) TO QFDB-DNAME-11.
           MOVE FDB-DOFFSET(11) TO QFDB-DOFFSET-11.
           MOVE FDB-DDIGITS(11) TO QFDB-DDIGITS-11.
           MOVE FDB-DTYPE(11) TO QFDB-DTYPE-11.
           MOVE FDB-DDECIMAL(11) TO QFDB-DDECIMAL-11.
           MOVE FDB-DNAME(12) TO QFDB-DNAME-12.
           MOVE FDB-DOFFSET(12) TO QFDB-DOFFSET-12.
           MOVE FDB-DDIGITS(12) TO QFDB-DDIGITS-12.
           MOVE FDB-DTYPE(12) TO QFDB-DTYPE-12.
           MOVE FDB-DDECIMAL(12) TO QFDB-DDECIMAL-12.
           MOVE FDB-DNAME(13) TO QFDB-DNAME-13.
           MOVE FDB-DOFFSET(13) TO QFDB-DOFFSET-13.
           MOVE FDB-DDIGITS(13) TO QFDB-DDIGITS-13.
           MOVE FDB-DTYPE(13) TO QFDB-DTYPE-13.
           MOVE FDB-DDECIMAL(13) TO QFDB-DDECIMAL-13.
           MOVE FDB-DNAME(14) TO QFDB-DNAME-14.
           MOVE FDB-DOFFSET(14) TO QFDB-DOFFSET-14.
           MOVE FDB-DDIGITS(14) TO QFDB-DDIGITS-14.
           MOVE FDB-DTYPE(14) TO QFDB-DTYPE-14.
           MOVE FDB-DDECIMAL(14) TO QFDB-DDECIMAL-14.
           MOVE FDB-DNAME(15) TO QFDB-DNAME-15.
           MOVE FDB-DOFFSET(15) TO QFDB-DOFFSET-15.
           MOVE FDB-DDIGITS(15) TO QFDB-DDIGITS-15.
           MOVE FDB-DTYPE(15) TO QFDB-DTYPE-15.
           MOVE FDB-DDECIMAL(15) TO QFDB-DDECIMAL-15.
           MOVE FDB-DNAME(16) TO QFDB-DNAME-16.
           MOVE FDB-DOFFSET(16) TO QFDB-DOFFSET-16.
           MOVE FDB-DDIGITS(16) TO QFDB-DDIGITS-16.
           MOVE FDB-DTYPE(16) TO QFDB-DTYPE-16.
           MOVE FDB-DDECIMAL(16) TO QFDB-DDECIMAL-16.
           MOVE FDB-DNAME(17) TO QFDB-DNAME-17.
           MOVE FDB-DOFFSET(17) TO QFDB-DOFFSET-17.
           MOVE FDB-DDIGITS(17) TO QFDB-DDIGITS-17.
           MOVE FDB-DTYPE(17) TO QFDB-DTYPE-17.
           MOVE FDB-DDECIMAL(17) TO QFDB-DDECIMAL-17.
           MOVE FDB-DNAME(18) TO QFDB-DNAME-18.
           MOVE FDB-DOFFSET(18) TO QFDB-DOFFSET-18.
           MOVE FDB-DDIGITS(18) TO QFDB-DDIGITS-18.
           MOVE FDB-DTYPE(18) TO QFDB-DTYPE-18.
           MOVE FDB-DDECIMAL(18) TO QFDB-DDECIMAL-18.
           MOVE FDB-DNAME(19) TO QFDB-DNAME-19.
           MOVE FDB-DOFFSET(19) TO QFDB-DOFFSET-19.
           MOVE FDB-DDIGITS(19) TO QFDB-DDIGITS-19.
           MOVE FDB-DTYPE(19) TO QFDB-DTYPE-19.
           MOVE FDB-DDECIMAL(19) TO QFDB-DDECIMAL-19.
           MOVE FDB-DNAME(20) TO QFDB-DNAME-20.
           MOVE FDB-DOFFSET(20) TO QFDB-DOFFSET-20.
           MOVE FDB-DDIGITS(20) TO QFDB-DDIGITS-20.
           MOVE FDB-DTYPE(20) TO QFDB-DTYPE-20.
           MOVE FDB-DDECIMAL(20) TO QFDB-DDECIMAL-20.
           MOVE FDB-DNAME(21) TO QFDB-DNAME-21.
           MOVE FDB-DOFFSET(21) TO QFDB-DOFFSET-21.
           MOVE FDB-DDIGITS(21) TO QFDB-DDIGITS-21.
           MOVE FDB-DTYPE(21) TO QFDB-DTYPE-21.
           MOVE FDB-DDECIMAL(21) TO QFDB-DDECIMAL-21.
           MOVE FDB-DNAME(22) TO QFDB-DNAME-22.
           MOVE FDB-DOFFSET(22) TO QFDB-DOFFSET-22.
           MOVE FDB-DDIGITS(22) TO QFDB-DDIGITS-22.
           MOVE FDB-DTYPE(22) TO QFDB-DTYPE-22.
           MOVE FDB-DDECIMAL(22) TO QFDB-DDECIMAL-22.
           MOVE FDB-DNAME(23) TO QFDB-DNAME-23.
           MOVE FDB-DOFFSET(23) TO QFDB-DOFFSET-23.
           MOVE FDB-DDIGITS(23) TO QFDB-DDIGITS-23.
           MOVE FDB-DTYPE(23) TO QFDB-DTYPE-23.
           MOVE FDB-DDECIMAL(23) TO QFDB-DDECIMAL-23.
           MOVE FDB-DNAME(24) TO QFDB-DNAME-24.
           MOVE FDB-DOFFSET(24) TO QFDB-DOFFSET-24.
           MOVE FDB-DDIGITS(24) TO QFDB-DDIGITS-24.
           MOVE FDB-DTYPE(24) TO QFDB-DTYPE-24.
           MOVE FDB-DDECIMAL(24) TO QFDB-DDECIMAL-24.
           MOVE FDB-DNAME(25) TO QFDB-DNAME-25.
           MOVE FDB-DOFFSET(25) TO QFDB-DOFFSET-25.
           MOVE FDB-DDIGITS(25) TO QFDB-DDIGITS-25.
           MOVE FDB-DTYPE(25) TO QFDB-DTYPE-25.
           MOVE FDB-DDECIMAL(25) TO QFDB-DDECIMAL-25.
           MOVE FDB-DNAME(26) TO QFDB-DNAME-26.
           MOVE FDB-DOFFSET(26) TO QFDB-DOFFSET-26.
           MOVE FDB-DDIGITS(26) TO QFDB-DDIGITS-26.
           MOVE FDB-DTYPE(26) TO QFDB-DTYPE-26.
           MOVE FDB-DDECIMAL(26) TO QFDB-DDECIMAL-26.
           MOVE FDB-DNAME(27) TO QFDB-DNAME-27.
           MOVE FDB-DOFFSET(27) TO QFDB-DOFFSET-27.
           MOVE FDB-DDIGITS(27) TO QFDB-DDIGITS-27.
           MOVE FDB-DTYPE(27) TO QFDB-DTYPE-27.
           MOVE FDB-DDECIMAL(27) TO QFDB-DDECIMAL-27.
           MOVE FDB-DNAME(28) TO QFDB-DNAME-28.
           MOVE FDB-DOFFSET(28) TO QFDB-DOFFSET-28.
           MOVE FDB-DDIGITS(28) TO QFDB-DDIGITS-28.
           MOVE FDB-DTYPE(28) TO QFDB-DTYPE-28.
           MOVE FDB-DDECIMAL(28) TO QFDB-DDECIMAL-28.
           MOVE FDB-DNAME(29) TO QFDB-DNAME-29.
           MOVE FDB-DOFFSET(29) TO QFDB-DOFFSET-29.
           MOVE FDB-DDIGITS(29) TO QFDB-DDIGITS-29.
           MOVE FDB-DTYPE(29) TO QFDB-DTYPE-29.
           MOVE FDB-DDECIMAL(29) TO QFDB-DDECIMAL-29.
           MOVE FDB-DNAME(30) TO QFDB-DNAME-30.
           MOVE FDB-DOFFSET(30) TO QFDB-DOFFSET-30.
           MOVE FDB-DDIGITS(30) TO QFDB-DDIGITS-30.
           MOVE FDB-DTYPE(30) TO QFDB-DTYPE-30.
           MOVE FDB-DDECIMAL(30) TO QFDB-DDECIMAL-30.
           MOVE FDB-DNAME(31) TO QFDB-DNAME-31.
           MOVE FDB-DOFFSET(31) TO QFDB-DOFFSET-31.
           MOVE FDB-DDIGITS(31) TO QFDB-DDIGITS-31.
           MOVE FDB-DTYPE(31) TO QFDB-DTYPE-31.
           MOVE FDB-DDECIMAL(31) TO QFDB-DDECIMAL-31.
           MOVE FDB-DNAME(32) TO QFDB-DNAME-32.
           MOVE FDB-DOFFSET(32) TO QFDB-DOFFSET-32.
           MOVE FDB-DDIGITS(32) TO QFDB-DDIGITS-32.
           MOVE FDB-DTYPE(32) TO QFDB-DTYPE-32.
           MOVE FDB-DDECIMAL(32) TO QFDB-DDECIMAL-32.
           MOVE FDB-DNAME(33) TO QFDB-DNAME-33.
           MOVE FDB-DOFFSET(33) TO QFDB-DOFFSET-33.
           MOVE FDB-DDIGITS(33) TO QFDB-DDIGITS-33.
           MOVE FDB-DTYPE(33) TO QFDB-DTYPE-33.
           MOVE FDB-DDECIMAL(33) TO QFDB-DDECIMAL-33.
           MOVE FDB-DNAME(34) TO QFDB-DNAME-34.
           MOVE FDB-DOFFSET(34) TO QFDB-DOFFSET-34.
           MOVE FDB-DDIGITS(34) TO QFDB-DDIGITS-34.
           MOVE FDB-DTYPE(34) TO QFDB-DTYPE-34.
           MOVE FDB-DDECIMAL(34) TO QFDB-DDECIMAL-34.
           MOVE FDB-DNAME(35) TO QFDB-DNAME-35.
           MOVE FDB-DOFFSET(35) TO QFDB-DOFFSET-35.
           MOVE FDB-DDIGITS(35) TO QFDB-DDIGITS-35.
           MOVE FDB-DTYPE(35) TO QFDB-DTYPE-35.
           MOVE FDB-DDECIMAL(35) TO QFDB-DDECIMAL-35.
           MOVE FDB-DNAME(36) TO QFDB-DNAME-36.
           MOVE FDB-DOFFSET(36) TO QFDB-DOFFSET-36.
           MOVE FDB-DDIGITS(36) TO QFDB-DDIGITS-36.
           MOVE FDB-DTYPE(36) TO QFDB-DTYPE-36.
           MOVE FDB-DDECIMAL(36) TO QFDB-DDECIMAL-36.
           MOVE FDB-DNAME(37) TO QFDB-DNAME-37.
           MOVE FDB-DOFFSET(37) TO QFDB-DOFFSET-37.
           MOVE FDB-DDIGITS(37) TO QFDB-DDIGITS-37.
           MOVE FDB-DTYPE(37) TO QFDB-DTYPE-37.
           MOVE FDB-DDECIMAL(37) TO QFDB-DDECIMAL-37.
           MOVE FDB-DNAME(38) TO QFDB-DNAME-38.
           MOVE FDB-DOFFSET(38) TO QFDB-DOFFSET-38.
           MOVE FDB-DDIGITS(38) TO QFDB-DDIGITS-38.
           MOVE FDB-DTYPE(38) TO QFDB-DTYPE-38.
           MOVE FDB-DDECIMAL(38) TO QFDB-DDECIMAL-38.
           MOVE FDB-DNAME(39) TO QFDB-DNAME-39.
           MOVE FDB-DOFFSET(39) TO QFDB-DOFFSET-39.
           MOVE FDB-DDIGITS(39) TO QFDB-DDIGITS-39.
           MOVE FDB-DTYPE(39) TO QFDB-DTYPE-39.
           MOVE FDB-DDECIMAL(39) TO QFDB-DDECIMAL-39.
           MOVE FDB-DNAME(40) TO QFDB-DNAME-40.
           MOVE FDB-DOFFSET(40) TO QFDB-DOFFSET-40.
           MOVE FDB-DDIGITS(40) TO QFDB-DDIGITS-40.
           MOVE FDB-DTYPE(40) TO QFDB-DTYPE-40.
           MOVE FDB-DDECIMAL(40) TO QFDB-DDECIMAL-40.
           MOVE FDB-DNAME(41) TO QFDB-DNAME-41.
           MOVE FDB-DOFFSET(41) TO QFDB-DOFFSET-41.
           MOVE FDB-DDIGITS(41) TO QFDB-DDIGITS-41.
           MOVE FDB-DTYPE(41) TO QFDB-DTYPE-41.
           MOVE FDB-DDECIMAL(41) TO QFDB-DDECIMAL-41.
           MOVE FDB-DNAME(42) TO QFDB-DNAME-42.
           MOVE FDB-DOFFSET(42) TO QFDB-DOFFSET-42.
           MOVE FDB-DDIGITS(42) TO QFDB-DDIGITS-42.
           MOVE FDB-DTYPE(42) TO QFDB-DTYPE-42.
           MOVE FDB-DDECIMAL(42) TO QFDB-DDECIMAL-42.
           MOVE FDB-DNAME(43) TO QFDB-DNAME-43.
           MOVE FDB-DOFFSET(43) TO QFDB-DOFFSET-43.
           MOVE FDB-DDIGITS(43) TO QFDB-DDIGITS-43.
           MOVE FDB-DTYPE(43) TO QFDB-DTYPE-43.
           MOVE FDB-DDECIMAL(43) TO QFDB-DDECIMAL-43.
           MOVE FDB-DNAME(44) TO QFDB-DNAME-44.
           MOVE FDB-DOFFSET(44) TO QFDB-DOFFSET-44.
           MOVE FDB-DDIGITS(44) TO QFDB-DDIGITS-44.
           MOVE FDB-DTYPE(44) TO QFDB-DTYPE-44.
           MOVE FDB-DDECIMAL(44) TO QFDB-DDECIMAL-44.
           MOVE FDB-DNAME(45) TO QFDB-DNAME-45.
           MOVE FDB-DOFFSET(45) TO QFDB-DOFFSET-45.
           MOVE FDB-DDIGITS(45) TO QFDB-DDIGITS-45.
           MOVE FDB-DTYPE(45) TO QFDB-DTYPE-45.
           MOVE FDB-DDECIMAL(45) TO QFDB-DDECIMAL-45.
           MOVE FDB-DNAME(46) TO QFDB-DNAME-46.
           MOVE FDB-DOFFSET(46) TO QFDB-DOFFSET-46.
           MOVE FDB-DDIGITS(46) TO QFDB-DDIGITS-46.
           MOVE FDB-DTYPE(46) TO QFDB-DTYPE-46.
           MOVE FDB-DDECIMAL(46) TO QFDB-DDECIMAL-46.
           MOVE FDB-DNAME(47) TO QFDB-DNAME-47.
           MOVE FDB-DOFFSET(47) TO QFDB-DOFFSET-47.
           MOVE FDB-DDIGITS(47) TO QFDB-DDIGITS-47.
           MOVE FDB-DTYPE(47) TO QFDB-DTYPE-47.
           MOVE FDB-DDECIMAL(47) TO QFDB-DDECIMAL-47.
           MOVE FDB-DNAME(48) TO QFDB-DNAME-48.
           MOVE FDB-DOFFSET(48) TO QFDB-DOFFSET-48.
           MOVE FDB-DDIGITS(48) TO QFDB-DDIGITS-48.
           MOVE FDB-DTYPE(48) TO QFDB-DTYPE-48.
           MOVE FDB-DDECIMAL(48) TO QFDB-DDECIMAL-48.
           MOVE FDB-DNAME(49) TO QFDB-DNAME-49.
           MOVE FDB-DOFFSET(49) TO QFDB-DOFFSET-49.
           MOVE FDB-DDIGITS(49) TO QFDB-DDIGITS-49.
           MOVE FDB-DTYPE(49) TO QFDB-DTYPE-49.
           MOVE FDB-DDECIMAL(49) TO QFDB-DDECIMAL-49.
           MOVE FDB-DNAME(50) TO QFDB-DNAME-50.
           MOVE FDB-DOFFSET(50) TO QFDB-DOFFSET-50.
           MOVE FDB-DDIGITS(50) TO QFDB-DDIGITS-50.
           MOVE FDB-DTYPE(50) TO QFDB-DTYPE-50.
           MOVE FDB-DDECIMAL(50) TO QFDB-DDECIMAL-50.
           MOVE FDB-DNAME(51) TO QFDB-DNAME-51.
           MOVE FDB-DOFFSET(51) TO QFDB-DOFFSET-51.
           MOVE FDB-DDIGITS(51) TO QFDB-DDIGITS-51.
           MOVE FDB-DTYPE(51) TO QFDB-DTYPE-51.
           MOVE FDB-DDECIMAL(51) TO QFDB-DDECIMAL-51.
           MOVE FDB-DNAME(52) TO QFDB-DNAME-52.
           MOVE FDB-DOFFSET(52) TO QFDB-DOFFSET-52.
           MOVE FDB-DDIGITS(52) TO QFDB-DDIGITS-52.
           MOVE FDB-DTYPE(52) TO QFDB-DTYPE-52.
           MOVE FDB-DDECIMAL(52) TO QFDB-DDECIMAL-52.
           MOVE FDB-DNAME(53) TO QFDB-DNAME-53.
           MOVE FDB-DOFFSET(53) TO QFDB-DOFFSET-53.
           MOVE FDB-DDIGITS(53) TO QFDB-DDIGITS-53.
           MOVE FDB-DTYPE(53) TO QFDB-DTYPE-53.
           MOVE FDB-DDECIMAL(53) TO QFDB-DDECIMAL-53.
           MOVE FDB-DNAME(54) TO QFDB-DNAME-54.
           MOVE FDB-DOFFSET(54) TO QFDB-DOFFSET-54.
           MOVE FDB-DDIGITS(54) TO QFDB-DDIGITS-54.
           MOVE FDB-DTYPE(54) TO QFDB-DTYPE-54.
           MOVE FDB-DDECIMAL(54) TO QFDB-DDECIMAL-54.
           MOVE FDB-DNAME(55) TO QFDB-DNAME-55.
           MOVE FDB-DOFFSET(55) TO QFDB-DOFFSET-55.
           MOVE FDB-DDIGITS(55) TO QFDB-DDIGITS-55.
           MOVE FDB-DTYPE(55) TO QFDB-DTYPE-55.
           MOVE FDB-DDECIMAL(55) TO QFDB-DDECIMAL-55.
           MOVE FDB-DNAME(56) TO QFDB-DNAME-56.
           MOVE FDB-DOFFSET(56) TO QFDB-DOFFSET-56.
           MOVE FDB-DDIGITS(56) TO QFDB-DDIGITS-56.
           MOVE FDB-DTYPE(56) TO QFDB-DTYPE-56.
           MOVE FDB-DDECIMAL(56) TO QFDB-DDECIMAL-56.
           MOVE FDB-DNAME(57) TO QFDB-DNAME-57.
           MOVE FDB-DOFFSET(57) TO QFDB-DOFFSET-57.
           MOVE FDB-DDIGITS(57) TO QFDB-DDIGITS-57.
           MOVE FDB-DTYPE(57) TO QFDB-DTYPE-57.
           MOVE FDB-DDECIMAL(57) TO QFDB-DDECIMAL-57.
           MOVE FDB-DNAME(58) TO QFDB-DNAME-58.
           MOVE FDB-DOFFSET(58) TO QFDB-DOFFSET-58.
           MOVE FDB-DDIGITS(58) TO QFDB-DDIGITS-58.
           MOVE FDB-DTYPE(58) TO QFDB-DTYPE-58.
           MOVE FDB-DDECIMAL(58) TO QFDB-DDECIMAL-58.
           MOVE FDB-DNAME(59) TO QFDB-DNAME-59.
           MOVE FDB-DOFFSET(59) TO QFDB-DOFFSET-59.
           MOVE FDB-DDIGITS(59) TO QFDB-DDIGITS-59.
           MOVE FDB-DTYPE(59) TO QFDB-DTYPE-59.
           MOVE FDB-DDECIMAL(59) TO QFDB-DDECIMAL-59.
           MOVE FDB-DNAME(60) TO QFDB-DNAME-60.
           MOVE FDB-DOFFSET(60) TO QFDB-DOFFSET-60.
           MOVE FDB-DDIGITS(60) TO QFDB-DDIGITS-60.
           MOVE FDB-DTYPE(60) TO QFDB-DTYPE-60.
           MOVE FDB-DDECIMAL(60) TO QFDB-DDECIMAL-60.
           MOVE FDB-DNAME(61) TO QFDB-DNAME-61.
           MOVE FDB-DOFFSET(61) TO QFDB-DOFFSET-61.
           MOVE FDB-DDIGITS(61) TO QFDB-DDIGITS-61.
           MOVE FDB-DTYPE(61) TO QFDB-DTYPE-61.
           MOVE FDB-DDECIMAL(61) TO QFDB-DDECIMAL-61.
           MOVE FDB-DNAME(62) TO QFDB-DNAME-62.
           MOVE FDB-DOFFSET(62) TO QFDB-DOFFSET-62.
           MOVE FDB-DDIGITS(62) TO QFDB-DDIGITS-62.
           MOVE FDB-DTYPE(62) TO QFDB-DTYPE-62.
           MOVE FDB-DDECIMAL(62) TO QFDB-DDECIMAL-62.
           MOVE FDB-DNAME(63) TO QFDB-DNAME-63.
           MOVE FDB-DOFFSET(63) TO QFDB-DOFFSET-63.
           MOVE FDB-DDIGITS(63) TO QFDB-DDIGITS-63.
           MOVE FDB-DTYPE(63) TO QFDB-DTYPE-63.
           MOVE FDB-DDECIMAL(63) TO QFDB-DDECIMAL-63.
           MOVE FDB-DNAME(64) TO QFDB-DNAME-64.
           MOVE FDB-DOFFSET(64) TO QFDB-DOFFSET-64.
           MOVE FDB-DDIGITS(64) TO QFDB-DDIGITS-64.
           MOVE FDB-DTYPE(64) TO QFDB-DTYPE-64.
           MOVE FDB-DDECIMAL(64) TO QFDB-DDECIMAL-64.
           MOVE FDB-DNAME(65) TO QFDB-DNAME-65.
           MOVE FDB-DOFFSET(65) TO QFDB-DOFFSET-65.
           MOVE FDB-DDIGITS(65) TO QFDB-DDIGITS-65.
           MOVE FDB-DTYPE(65) TO QFDB-DTYPE-65.
           MOVE FDB-DDECIMAL(65) TO QFDB-DDECIMAL-65.
           MOVE FDB-DNAME(66) TO QFDB-DNAME-66.
           MOVE FDB-DOFFSET(66) TO QFDB-DOFFSET-66.
           MOVE FDB-DDIGITS(66) TO QFDB-DDIGITS-66.
           MOVE FDB-DTYPE(66) TO QFDB-DTYPE-66.
           MOVE FDB-DDECIMAL(66) TO QFDB-DDECIMAL-66.
           MOVE FDB-DNAME(67) TO QFDB-DNAME-67.
           MOVE FDB-DOFFSET(67) TO QFDB-DOFFSET-67.
           MOVE FDB-DDIGITS(67) TO QFDB-DDIGITS-67.
           MOVE FDB-DTYPE(67) TO QFDB-DTYPE-67.
           MOVE FDB-DDECIMAL(67) TO QFDB-DDECIMAL-67.
           MOVE FDB-DNAME(68) TO QFDB-DNAME-68.
           MOVE FDB-DOFFSET(68) TO QFDB-DOFFSET-68.
           MOVE FDB-DDIGITS(68) TO QFDB-DDIGITS-68.
           MOVE FDB-DTYPE(68) TO QFDB-DTYPE-68.
           MOVE FDB-DDECIMAL(68) TO QFDB-DDECIMAL-68.
           MOVE FDB-DNAME(69) TO QFDB-DNAME-69.
           MOVE FDB-DOFFSET(69) TO QFDB-DOFFSET-69.
           MOVE FDB-DDIGITS(69) TO QFDB-DDIGITS-69.
           MOVE FDB-DTYPE(69) TO QFDB-DTYPE-69.
           MOVE FDB-DDECIMAL(69) TO QFDB-DDECIMAL-69.
           MOVE FDB-DNAME(70) TO QFDB-DNAME-70.
           MOVE FDB-DOFFSET(70) TO QFDB-DOFFSET-70.
           MOVE FDB-DDIGITS(70) TO QFDB-DDIGITS-70.
           MOVE FDB-DTYPE(70) TO QFDB-DTYPE-70.
           MOVE FDB-DDECIMAL(70) TO QFDB-DDECIMAL-70.
           MOVE FDB-DNAME(71) TO QFDB-DNAME-71.
           MOVE FDB-DOFFSET(71) TO QFDB-DOFFSET-71.
           MOVE FDB-DDIGITS(71) TO QFDB-DDIGITS-71.
           MOVE FDB-DTYPE(71) TO QFDB-DTYPE-71.
           MOVE FDB-DDECIMAL(71) TO QFDB-DDECIMAL-71.
           MOVE FDB-DNAME(72) TO QFDB-DNAME-72.
           MOVE FDB-DOFFSET(72) TO QFDB-DOFFSET-72.
           MOVE FDB-DDIGITS(72) TO QFDB-DDIGITS-72.
           MOVE FDB-DTYPE(72) TO QFDB-DTYPE-72.
           MOVE FDB-DDECIMAL(72) TO QFDB-DDECIMAL-72.
           MOVE FDB-DNAME(73) TO QFDB-DNAME-73.
           MOVE FDB-DOFFSET(73) TO QFDB-DOFFSET-73.
           MOVE FDB-DDIGITS(73) TO QFDB-DDIGITS-73.
           MOVE FDB-DTYPE(73) TO QFDB-DTYPE-73.
           MOVE FDB-DDECIMAL(73) TO QFDB-DDECIMAL-73.
           MOVE FDB-DNAME(74) TO QFDB-DNAME-74.
           MOVE FDB-DOFFSET(74) TO QFDB-DOFFSET-74.
           MOVE FDB-DDIGITS(74) TO QFDB-DDIGITS-74.
           MOVE FDB-DTYPE(74) TO QFDB-DTYPE-74.
           MOVE FDB-DDECIMAL(74) TO QFDB-DDECIMAL-74.
           MOVE FDB-DNAME(75) TO QFDB-DNAME-75.
           MOVE FDB-DOFFSET(75) TO QFDB-DOFFSET-75.
           MOVE FDB-DDIGITS(75) TO QFDB-DDIGITS-75.
           MOVE FDB-DTYPE(75) TO QFDB-DTYPE-75.
           MOVE FDB-DDECIMAL(75) TO QFDB-DDECIMAL-75.
           MOVE FDB-DNAME(76) TO QFDB-DNAME-76.
           MOVE FDB-DOFFSET(76) TO QFDB-DOFFSET-76.
           MOVE FDB-DDIGITS(76) TO QFDB-DDIGITS-76.
           MOVE FDB-DTYPE(76) TO QFDB-DTYPE-76.
           MOVE FDB-DDECIMAL(76) TO QFDB-DDECIMAL-76.
           MOVE FDB-DNAME(77) TO QFDB-DNAME-77.
           MOVE FDB-DOFFSET(77) TO QFDB-DOFFSET-77.
           MOVE FDB-DDIGITS(77) TO QFDB-DDIGITS-77.
           MOVE FDB-DTYPE(77) TO QFDB-DTYPE-77.
           MOVE FDB-DDECIMAL(77) TO QFDB-DDECIMAL-77.
           MOVE FDB-DNAME(78) TO QFDB-DNAME-78.
           MOVE FDB-DOFFSET(78) TO QFDB-DOFFSET-78.
           MOVE FDB-DDIGITS(78) TO QFDB-DDIGITS-78.
           MOVE FDB-DTYPE(78) TO QFDB-DTYPE-78.
           MOVE FDB-DDECIMAL(78) TO QFDB-DDECIMAL-78.
           MOVE FDB-DNAME(79) TO QFDB-DNAME-79.
           MOVE FDB-DOFFSET(79) TO QFDB-DOFFSET-79.
           MOVE FDB-DDIGITS(79) TO QFDB-DDIGITS-79.
           MOVE FDB-DTYPE(79) TO QFDB-DTYPE-79.
           MOVE FDB-DDECIMAL(79) TO QFDB-DDECIMAL-79.
           MOVE FDB-DNAME(80) TO QFDB-DNAME-80.
           MOVE FDB-DOFFSET(80) TO QFDB-DOFFSET-80.
           MOVE FDB-DDIGITS(80) TO QFDB-DDIGITS-80.
           MOVE FDB-DTYPE(80) TO QFDB-DTYPE-80.
           MOVE FDB-DDECIMAL(80) TO QFDB-DDECIMAL-80.
           MOVE FDB-DNAME(81) TO QFDB-DNAME-81.
           MOVE FDB-DOFFSET(81) TO QFDB-DOFFSET-81.
           MOVE FDB-DDIGITS(81) TO QFDB-DDIGITS-81.
           MOVE FDB-DTYPE(81) TO QFDB-DTYPE-81.
           MOVE FDB-DDECIMAL(81) TO QFDB-DDECIMAL-81.
           MOVE FDB-DNAME(82) TO QFDB-DNAME-82.
           MOVE FDB-DOFFSET(82) TO QFDB-DOFFSET-82.
           MOVE FDB-DDIGITS(82) TO QFDB-DDIGITS-82.
           MOVE FDB-DTYPE(82) TO QFDB-DTYPE-82.
           MOVE FDB-DDECIMAL(82) TO QFDB-DDECIMAL-82.
           MOVE FDB-DNAME(83) TO QFDB-DNAME-83.
           MOVE FDB-DOFFSET(83) TO QFDB-DOFFSET-83.
           MOVE FDB-DDIGITS(83) TO QFDB-DDIGITS-83.
           MOVE FDB-DTYPE(83) TO QFDB-DTYPE-83.
           MOVE FDB-DDECIMAL(83) TO QFDB-DDECIMAL-83.
           MOVE FDB-DNAME(84) TO QFDB-DNAME-84.
           MOVE FDB-DOFFSET(84) TO QFDB-DOFFSET-84.
           MOVE FDB-DDIGITS(84) TO QFDB-DDIGITS-84.
           MOVE FDB-DTYPE(84) TO QFDB-DTYPE-84.
           MOVE FDB-DDECIMAL(84) TO QFDB-DDECIMAL-84.
           MOVE FDB-DNAME(85) TO QFDB-DNAME-85.
           MOVE FDB-DOFFSET(85) TO QFDB-DOFFSET-85.
           MOVE FDB-DDIGITS(85) TO QFDB-DDIGITS-85.
           MOVE FDB-DTYPE(85) TO QFDB-DTYPE-85.
           MOVE FDB-DDECIMAL(85) TO QFDB-DDECIMAL-85.
           MOVE FDB-DNAME(86) TO QFDB-DNAME-86.
           MOVE FDB-DOFFSET(86) TO QFDB-DOFFSET-86.
           MOVE FDB-DDIGITS(86) TO QFDB-DDIGITS-86.
           MOVE FDB-DTYPE(86) TO QFDB-DTYPE-86.
           MOVE FDB-DDECIMAL(86) TO QFDB-DDECIMAL-86.
           MOVE FDB-DNAME(87) TO QFDB-DNAME-87.
           MOVE FDB-DOFFSET(87) TO QFDB-DOFFSET-87.
           MOVE FDB-DDIGITS(87) TO QFDB-DDIGITS-87.
           MOVE FDB-DTYPE(87) TO QFDB-DTYPE-87.
           MOVE FDB-DDECIMAL(87) TO QFDB-DDECIMAL-87.
           MOVE FDB-DNAME(88) TO QFDB-DNAME-88.
           MOVE FDB-DOFFSET(88) TO QFDB-DOFFSET-88.
           MOVE FDB-DDIGITS(88) TO QFDB-DDIGITS-88.
           MOVE FDB-DTYPE(88) TO QFDB-DTYPE-88.
           MOVE FDB-DDECIMAL(88) TO QFDB-DDECIMAL-88.
           MOVE FDB-DNAME(89) TO QFDB-DNAME-89.
           MOVE FDB-DOFFSET(89) TO QFDB-DOFFSET-89.
           MOVE FDB-DDIGITS(89) TO QFDB-DDIGITS-89.
           MOVE FDB-DTYPE(89) TO QFDB-DTYPE-89.
           MOVE FDB-DDECIMAL(89) TO QFDB-DDECIMAL-89.
           MOVE FDB-DNAME(90) TO QFDB-DNAME-90.
           MOVE FDB-DOFFSET(90) TO QFDB-DOFFSET-90.
           MOVE FDB-DDIGITS(90) TO QFDB-DDIGITS-90.
           MOVE FDB-DTYPE(90) TO QFDB-DTYPE-90.
           MOVE FDB-DDECIMAL(90) TO QFDB-DDECIMAL-90.
           MOVE FDB-DNAME(91) TO QFDB-DNAME-91.
           MOVE FDB-DOFFSET(91) TO QFDB-DOFFSET-91.
           MOVE FDB-DDIGITS(91) TO QFDB-DDIGITS-91.
           MOVE FDB-DTYPE(91) TO QFDB-DTYPE-91.
           MOVE FDB-DDECIMAL(91) TO QFDB-DDECIMAL-91.
           MOVE FDB-DNAME(92) TO QFDB-DNAME-92.
           MOVE FDB-DOFFSET(92) TO QFDB-DOFFSET-92.
           MOVE FDB-DDIGITS(92) TO QFDB-DDIGITS-92.
           MOVE FDB-DTYPE(92) TO QFDB-DTYPE-92.
           MOVE FDB-DDECIMAL(92) TO QFDB-DDECIMAL-92.
           MOVE FDB-DNAME(93) TO QFDB-DNAME-93.
           MOVE FDB-DOFFSET(93) TO QFDB-DOFFSET-93.
           MOVE FDB-DDIGITS(93) TO QFDB-DDIGITS-93.
           MOVE FDB-DTYPE(93) TO QFDB-DTYPE-93.
           MOVE FDB-DDECIMAL(93) TO QFDB-DDECIMAL-93.
           MOVE FDB-DNAME(94) TO QFDB-DNAME-94.
           MOVE FDB-DOFFSET(94) TO QFDB-DOFFSET-94.
           MOVE FDB-DDIGITS(94) TO QFDB-DDIGITS-94.
           MOVE FDB-DTYPE(94) TO QFDB-DTYPE-94.
           MOVE FDB-DDECIMAL(94) TO QFDB-DDECIMAL-94.
           MOVE FDB-DNAME(95) TO QFDB-DNAME-95.
           MOVE FDB-DOFFSET(95) TO QFDB-DOFFSET-95.
           MOVE FDB-DDIGITS(95) TO QFDB-DDIGITS-95.
           MOVE FDB-DTYPE(95) TO QFDB-DTYPE-95.
           MOVE FDB-DDECIMAL(95) TO QFDB-DDECIMAL-95.
           MOVE FDB-DNAME(96) TO QFDB-DNAME-96.
           MOVE FDB-DOFFSET(96) TO QFDB-DOFFSET-96.
           MOVE FDB-DDIGITS(96) TO QFDB-DDIGITS-96.
           MOVE FDB-DTYPE(96) TO QFDB-DTYPE-96.
           MOVE FDB-DDECIMAL(96) TO QFDB-DDECIMAL-96.
           MOVE FDB-DNAME(97) TO QFDB-DNAME-97.
           MOVE FDB-DOFFSET(97) TO QFDB-DOFFSET-97.
           MOVE FDB-DDIGITS(97) TO QFDB-DDIGITS-97.
           MOVE FDB-DTYPE(97) TO QFDB-DTYPE-97.
           MOVE FDB-DDECIMAL(97) TO QFDB-DDECIMAL-97.
           MOVE FDB-DNAME(98) TO QFDB-DNAME-98.
           MOVE FDB-DOFFSET(98) TO QFDB-DOFFSET-98.
           MOVE FDB-DDIGITS(98) TO QFDB-DDIGITS-98.
           MOVE FDB-DTYPE(98) TO QFDB-DTYPE-98.
           MOVE FDB-DDECIMAL(98) TO QFDB-DDECIMAL-98.
           MOVE FDB-DNAME(99) TO QFDB-DNAME-99.
           MOVE FDB-DOFFSET(99) TO QFDB-DOFFSET-99.
           MOVE FDB-DDIGITS(99) TO QFDB-DDIGITS-99.
           MOVE FDB-DTYPE(99) TO QFDB-DTYPE-99.
           MOVE FDB-DDECIMAL(99) TO QFDB-DDECIMAL-99.
           MOVE FDB-DNAME(100) TO QFDB-DNAME-100.
           MOVE FDB-DOFFSET(100) TO QFDB-DOFFSET-100.
           MOVE FDB-DDIGITS(100) TO QFDB-DDIGITS-100.
           MOVE FDB-DTYPE(100) TO QFDB-DTYPE-100.
           MOVE FDB-DDECIMAL(100) TO QFDB-DDECIMAL-100.
           MOVE FDB-DNAME(101) TO QFDB-DNAME-101.
           MOVE FDB-DOFFSET(101) TO QFDB-DOFFSET-101.
           MOVE FDB-DDIGITS(101) TO QFDB-DDIGITS-101.
           MOVE FDB-DTYPE(101) TO QFDB-DTYPE-101.
           MOVE FDB-DDECIMAL(101) TO QFDB-DDECIMAL-101.
           MOVE FDB-DNAME(102) TO QFDB-DNAME-102.
           MOVE FDB-DOFFSET(102) TO QFDB-DOFFSET-102.
           MOVE FDB-DDIGITS(102) TO QFDB-DDIGITS-102.
           MOVE FDB-DTYPE(102) TO QFDB-DTYPE-102.
           MOVE FDB-DDECIMAL(102) TO QFDB-DDECIMAL-102.
           MOVE FDB-DNAME(103) TO QFDB-DNAME-103.
           MOVE FDB-DOFFSET(103) TO QFDB-DOFFSET-103.
           MOVE FDB-DDIGITS(103) TO QFDB-DDIGITS-103.
           MOVE FDB-DTYPE(103) TO QFDB-DTYPE-103.
           MOVE FDB-DDECIMAL(103) TO QFDB-DDECIMAL-103.
           MOVE FDB-DNAME(104) TO QFDB-DNAME-104.
           MOVE FDB-DOFFSET(104) TO QFDB-DOFFSET-104.
           MOVE FDB-DDIGITS(104) TO QFDB-DDIGITS-104.
           MOVE FDB-DTYPE(104) TO QFDB-DTYPE-104.
           MOVE FDB-DDECIMAL(104) TO QFDB-DDECIMAL-104.
           MOVE FDB-DNAME(105) TO QFDB-DNAME-105.
           MOVE FDB-DOFFSET(105) TO QFDB-DOFFSET-105.
           MOVE FDB-DDIGITS(105) TO QFDB-DDIGITS-105.
           MOVE FDB-DTYPE(105) TO QFDB-DTYPE-105.
           MOVE FDB-DDECIMAL(105) TO QFDB-DDECIMAL-105.
           MOVE FDB-DNAME(106) TO QFDB-DNAME-106.
           MOVE FDB-DOFFSET(106) TO QFDB-DOFFSET-106.
           MOVE FDB-DDIGITS(106) TO QFDB-DDIGITS-106.
           MOVE FDB-DTYPE(106) TO QFDB-DTYPE-106.
           MOVE FDB-DDECIMAL(106) TO QFDB-DDECIMAL-106.
           MOVE FDB-DNAME(107) TO QFDB-DNAME-107.
           MOVE FDB-DOFFSET(107) TO QFDB-DOFFSET-107.
           MOVE FDB-DDIGITS(107) TO QFDB-DDIGITS-107.
           MOVE FDB-DTYPE(107) TO QFDB-DTYPE-107.
           MOVE FDB-DDECIMAL(107) TO QFDB-DDECIMAL-107.
           MOVE FDB-DNAME(108) TO QFDB-DNAME-108.
           MOVE FDB-DOFFSET(108) TO QFDB-DOFFSET-108.
           MOVE FDB-DDIGITS(108) TO QFDB-DDIGITS-108.
           MOVE FDB-DTYPE(108) TO QFDB-DTYPE-108.
           MOVE FDB-DDECIMAL(108) TO QFDB-DDECIMAL-108.
           MOVE FDB-DNAME(109) TO QFDB-DNAME-109.
           MOVE FDB-DOFFSET(109) TO QFDB-DOFFSET-109.
           MOVE FDB-DDIGITS(109) TO QFDB-DDIGITS-109.
           MOVE FDB-DTYPE(109) TO QFDB-DTYPE-109.
           MOVE FDB-DDECIMAL(109) TO QFDB-DDECIMAL-109.
           MOVE FDB-DNAME(110) TO QFDB-DNAME-110.
           MOVE FDB-DOFFSET(110) TO QFDB-DOFFSET-110.
           MOVE FDB-DDIGITS(110) TO QFDB-DDIGITS-110.
           MOVE FDB-DTYPE(110) TO QFDB-DTYPE-110.
           MOVE FDB-DDECIMAL(110) TO QFDB-DDECIMAL-110.
           MOVE FDB-DNAME(111) TO QFDB-DNAME-111.
           MOVE FDB-DOFFSET(111) TO QFDB-DOFFSET-111.
           MOVE FDB-DDIGITS(111) TO QFDB-DDIGITS-111.
           MOVE FDB-DTYPE(111) TO QFDB-DTYPE-111.
           MOVE FDB-DDECIMAL(111) TO QFDB-DDECIMAL-111.
           MOVE FDB-DNAME(112) TO QFDB-DNAME-112.
           MOVE FDB-DOFFSET(112) TO QFDB-DOFFSET-112.
           MOVE FDB-DDIGITS(112) TO QFDB-DDIGITS-112.
           MOVE FDB-DTYPE(112) TO QFDB-DTYPE-112.
           MOVE FDB-DDECIMAL(112) TO QFDB-DDECIMAL-112.
           MOVE FDB-DNAME(113) TO QFDB-DNAME-113.
           MOVE FDB-DOFFSET(113) TO QFDB-DOFFSET-113.
           MOVE FDB-DDIGITS(113) TO QFDB-DDIGITS-113.
           MOVE FDB-DTYPE(113) TO QFDB-DTYPE-113.
           MOVE FDB-DDECIMAL(113) TO QFDB-DDECIMAL-113.
           MOVE FDB-DNAME(114) TO QFDB-DNAME-114.
           MOVE FDB-DOFFSET(114) TO QFDB-DOFFSET-114.
           MOVE FDB-DDIGITS(114) TO QFDB-DDIGITS-114.
           MOVE FDB-DTYPE(114) TO QFDB-DTYPE-114.
           MOVE FDB-DDECIMAL(114) TO QFDB-DDECIMAL-114.
           MOVE FDB-DNAME(115) TO QFDB-DNAME-115.
           MOVE FDB-DOFFSET(115) TO QFDB-DOFFSET-115.
           MOVE FDB-DDIGITS(115) TO QFDB-DDIGITS-115.
           MOVE FDB-DTYPE(115) TO QFDB-DTYPE-115.
           MOVE FDB-DDECIMAL(115) TO QFDB-DDECIMAL-115.
           MOVE FDB-DNAME(116) TO QFDB-DNAME-116.
           MOVE FDB-DOFFSET(116) TO QFDB-DOFFSET-116.
           MOVE FDB-DDIGITS(116) TO QFDB-DDIGITS-116.
           MOVE FDB-DTYPE(116) TO QFDB-DTYPE-116.
           MOVE FDB-DDECIMAL(116) TO QFDB-DDECIMAL-116.
           MOVE FDB-DNAME(117) TO QFDB-DNAME-117.
           MOVE FDB-DOFFSET(117) TO QFDB-DOFFSET-117.
           MOVE FDB-DDIGITS(117) TO QFDB-DDIGITS-117.
           MOVE FDB-DTYPE(117) TO QFDB-DTYPE-117.
           MOVE FDB-DDECIMAL(117) TO QFDB-DDECIMAL-117.
           MOVE FDB-DNAME(118) TO QFDB-DNAME-118.
           MOVE FDB-DOFFSET(118) TO QFDB-DOFFSET-118.
           MOVE FDB-DDIGITS(118) TO QFDB-DDIGITS-118.
           MOVE FDB-DTYPE(118) TO QFDB-DTYPE-118.
           MOVE FDB-DDECIMAL(118) TO QFDB-DDECIMAL-118.
           MOVE FDB-DNAME(119) TO QFDB-DNAME-119.
           MOVE FDB-DOFFSET(119) TO QFDB-DOFFSET-119.
           MOVE FDB-DDIGITS(119) TO QFDB-DDIGITS-119.
           MOVE FDB-DTYPE(119) TO QFDB-DTYPE-119.
           MOVE FDB-DDECIMAL(119) TO QFDB-DDECIMAL-119.
           MOVE FDB-DNAME(120) TO QFDB-DNAME-120.
           MOVE FDB-DOFFSET(120) TO QFDB-DOFFSET-120.
           MOVE FDB-DDIGITS(120) TO QFDB-DDIGITS-120.
           MOVE FDB-DTYPE(120) TO QFDB-DTYPE-120.
           MOVE FDB-DDECIMAL(120) TO QFDB-DDECIMAL-120.
           MOVE FDB-DNAME(121) TO QFDB-DNAME-121.
           MOVE FDB-DOFFSET(121) TO QFDB-DOFFSET-121.
           MOVE FDB-DDIGITS(121) TO QFDB-DDIGITS-121.
           MOVE FDB-DTYPE(121) TO QFDB-DTYPE-121.
           MOVE FDB-DDECIMAL(121) TO QFDB-DDECIMAL-121.
           MOVE FDB-DNAME(122) TO QFDB-DNAME-122.
           MOVE FDB-DOFFSET(122) TO QFDB-DOFFSET-122.
           MOVE FDB-DDIGITS(122) TO QFDB-DDIGITS-122.
           MOVE FDB-DTYPE(122) TO QFDB-DTYPE-122.
           MOVE FDB-DDECIMAL(122) TO QFDB-DDECIMAL-122.
           MOVE FDB-DNAME(123) TO QFDB-DNAME-123.
           MOVE FDB-DOFFSET(123) TO QFDB-DOFFSET-123.
           MOVE FDB-DDIGITS(123) TO QFDB-DDIGITS-123.
           MOVE FDB-DTYPE(123) TO QFDB-DTYPE-123.
           MOVE FDB-DDECIMAL(123) TO QFDB-DDECIMAL-123.
           MOVE FDB-DNAME(124) TO QFDB-DNAME-124.
           MOVE FDB-DOFFSET(124) TO QFDB-DOFFSET-124.
           MOVE FDB-DDIGITS(124) TO QFDB-DDIGITS-124.
           MOVE FDB-DTYPE(124) TO QFDB-DTYPE-124.
           MOVE FDB-DDECIMAL(124) TO QFDB-DDECIMAL-124.
           MOVE FDB-DNAME(125) TO QFDB-DNAME-125.
           MOVE FDB-DOFFSET(125) TO QFDB-DOFFSET-125.
           MOVE FDB-DDIGITS(125) TO QFDB-DDIGITS-125.
           MOVE FDB-DTYPE(125) TO QFDB-DTYPE-125.
           MOVE FDB-DDECIMAL(125) TO QFDB-DDECIMAL-125.
           MOVE FDB-DNAME(126) TO QFDB-DNAME-126.
           MOVE FDB-DOFFSET(126) TO QFDB-DOFFSET-126.
           MOVE FDB-DDIGITS(126) TO QFDB-DDIGITS-126.
           MOVE FDB-DTYPE(126) TO QFDB-DTYPE-126.
           MOVE FDB-DDECIMAL(126) TO QFDB-DDECIMAL-126.
           MOVE FDB-DNAME(127) TO QFDB-DNAME-127.
           MOVE FDB-DOFFSET(127) TO QFDB-DOFFSET-127.
           MOVE FDB-DDIGITS(127) TO QFDB-DDIGITS-127.
           MOVE FDB-DTYPE(127) TO QFDB-DTYPE-127.
           MOVE FDB-DDECIMAL(127) TO QFDB-DDECIMAL-127.
           MOVE FDB-DNAME(128) TO QFDB-DNAME-128.
           MOVE FDB-DOFFSET(128) TO QFDB-DOFFSET-128.
           MOVE FDB-DDIGITS(128) TO QFDB-DDIGITS-128.
           MOVE FDB-DTYPE(128) TO QFDB-DTYPE-128.
           MOVE FDB-DDECIMAL(128) TO QFDB-DDECIMAL-128.
           MOVE FDB-DNAME(129) TO QFDB-DNAME-129.
           MOVE FDB-DOFFSET(129) TO QFDB-DOFFSET-129.
           MOVE FDB-DDIGITS(129) TO QFDB-DDIGITS-129.
           MOVE FDB-DTYPE(129) TO QFDB-DTYPE-129.
           MOVE FDB-DDECIMAL(129) TO QFDB-DDECIMAL-129.
           MOVE FDB-DNAME(130) TO QFDB-DNAME-130.
           MOVE FDB-DOFFSET(130) TO QFDB-DOFFSET-130.
           MOVE FDB-DDIGITS(130) TO QFDB-DDIGITS-130.
           MOVE FDB-DTYPE(130) TO QFDB-DTYPE-130.
           MOVE FDB-DDECIMAL(130) TO QFDB-DDECIMAL-130.
           MOVE FDB-DNAME(131) TO QFDB-DNAME-131.
           MOVE FDB-DOFFSET(131) TO QFDB-DOFFSET-131.
           MOVE FDB-DDIGITS(131) TO QFDB-DDIGITS-131.
           MOVE FDB-DTYPE(131) TO QFDB-DTYPE-131.
           MOVE FDB-DDECIMAL(131) TO QFDB-DDECIMAL-131.
           MOVE FDB-DNAME(132) TO QFDB-DNAME-132.
           MOVE FDB-DOFFSET(132) TO QFDB-DOFFSET-132.
           MOVE FDB-DDIGITS(132) TO QFDB-DDIGITS-132.
           MOVE FDB-DTYPE(132) TO QFDB-DTYPE-132.
           MOVE FDB-DDECIMAL(132) TO QFDB-DDECIMAL-132.
           MOVE FDB-DNAME(133) TO QFDB-DNAME-133.
           MOVE FDB-DOFFSET(133) TO QFDB-DOFFSET-133.
           MOVE FDB-DDIGITS(133) TO QFDB-DDIGITS-133.
           MOVE FDB-DTYPE(133) TO QFDB-DTYPE-133.
           MOVE FDB-DDECIMAL(133) TO QFDB-DDECIMAL-133.
           MOVE FDB-DNAME(134) TO QFDB-DNAME-134.
           MOVE FDB-DOFFSET(134) TO QFDB-DOFFSET-134.
           MOVE FDB-DDIGITS(134) TO QFDB-DDIGITS-134.
           MOVE FDB-DTYPE(134) TO QFDB-DTYPE-134.
           MOVE FDB-DDECIMAL(134) TO QFDB-DDECIMAL-134.
           MOVE FDB-DNAME(135) TO QFDB-DNAME-135.
           MOVE FDB-DOFFSET(135) TO QFDB-DOFFSET-135.
           MOVE FDB-DDIGITS(135) TO QFDB-DDIGITS-135.
           MOVE FDB-DTYPE(135) TO QFDB-DTYPE-135.
           MOVE FDB-DDECIMAL(135) TO QFDB-DDECIMAL-135.
           MOVE FDB-DNAME(136) TO QFDB-DNAME-136.
           MOVE FDB-DOFFSET(136) TO QFDB-DOFFSET-136.
           MOVE FDB-DDIGITS(136) TO QFDB-DDIGITS-136.
           MOVE FDB-DTYPE(136) TO QFDB-DTYPE-136.
           MOVE FDB-DDECIMAL(136) TO QFDB-DDECIMAL-136.
           MOVE FDB-DNAME(137) TO QFDB-DNAME-137.
           MOVE FDB-DOFFSET(137) TO QFDB-DOFFSET-137.
           MOVE FDB-DDIGITS(137) TO QFDB-DDIGITS-137.
           MOVE FDB-DTYPE(137) TO QFDB-DTYPE-137.
           MOVE FDB-DDECIMAL(137) TO QFDB-DDECIMAL-137.
           MOVE FDB-DNAME(138) TO QFDB-DNAME-138.
           MOVE FDB-DOFFSET(138) TO QFDB-DOFFSET-138.
           MOVE FDB-DDIGITS(138) TO QFDB-DDIGITS-138.
           MOVE FDB-DTYPE(138) TO QFDB-DTYPE-138.
           MOVE FDB-DDECIMAL(138) TO QFDB-DDECIMAL-138.
           MOVE FDB-DNAME(139) TO QFDB-DNAME-139.
           MOVE FDB-DOFFSET(139) TO QFDB-DOFFSET-139.
           MOVE FDB-DDIGITS(139) TO QFDB-DDIGITS-139.
           MOVE FDB-DTYPE(139) TO QFDB-DTYPE-139.
           MOVE FDB-DDECIMAL(139) TO QFDB-DDECIMAL-139.
           MOVE FDB-DNAME(140) TO QFDB-DNAME-140.
           MOVE FDB-DOFFSET(140) TO QFDB-DOFFSET-140.
           MOVE FDB-DDIGITS(140) TO QFDB-DDIGITS-140.
           MOVE FDB-DTYPE(140) TO QFDB-DTYPE-140.
           MOVE FDB-DDECIMAL(140) TO QFDB-DDECIMAL-140.
           MOVE FDB-DNAME(141) TO QFDB-DNAME-141.
           MOVE FDB-DOFFSET(141) TO QFDB-DOFFSET-141.
           MOVE FDB-DDIGITS(141) TO QFDB-DDIGITS-141.
           MOVE FDB-DTYPE(141) TO QFDB-DTYPE-141.
           MOVE FDB-DDECIMAL(141) TO QFDB-DDECIMAL-141.
           MOVE FDB-DNAME(142) TO QFDB-DNAME-142.
           MOVE FDB-DOFFSET(142) TO QFDB-DOFFSET-142.
           MOVE FDB-DDIGITS(142) TO QFDB-DDIGITS-142.
           MOVE FDB-DTYPE(142) TO QFDB-DTYPE-142.
           MOVE FDB-DDECIMAL(142) TO QFDB-DDECIMAL-142.
           MOVE FDB-DNAME(143) TO QFDB-DNAME-143.
           MOVE FDB-DOFFSET(143) TO QFDB-DOFFSET-143.
           MOVE FDB-DDIGITS(143) TO QFDB-DDIGITS-143.
           MOVE FDB-DTYPE(143) TO QFDB-DTYPE-143.
           MOVE FDB-DDECIMAL(143) TO QFDB-DDECIMAL-143.
           MOVE FDB-DNAME(144) TO QFDB-DNAME-144.
           MOVE FDB-DOFFSET(144) TO QFDB-DOFFSET-144.
           MOVE FDB-DDIGITS(144) TO QFDB-DDIGITS-144.
           MOVE FDB-DTYPE(144) TO QFDB-DTYPE-144.
           MOVE FDB-DDECIMAL(144) TO QFDB-DDECIMAL-144.
           MOVE FDB-DNAME(145) TO QFDB-DNAME-145.
           MOVE FDB-DOFFSET(145) TO QFDB-DOFFSET-145.
           MOVE FDB-DDIGITS(145) TO QFDB-DDIGITS-145.
           MOVE FDB-DTYPE(145) TO QFDB-DTYPE-145.
           MOVE FDB-DDECIMAL(145) TO QFDB-DDECIMAL-145.
           MOVE FDB-DNAME(146) TO QFDB-DNAME-146.
           MOVE FDB-DOFFSET(146) TO QFDB-DOFFSET-146.
           MOVE FDB-DDIGITS(146) TO QFDB-DDIGITS-146.
           MOVE FDB-DTYPE(146) TO QFDB-DTYPE-146.
           MOVE FDB-DDECIMAL(146) TO QFDB-DDECIMAL-146.
           MOVE FDB-DNAME(147) TO QFDB-DNAME-147.
           MOVE FDB-DOFFSET(147) TO QFDB-DOFFSET-147.
           MOVE FDB-DDIGITS(147) TO QFDB-DDIGITS-147.
           MOVE FDB-DTYPE(147) TO QFDB-DTYPE-147.
           MOVE FDB-DDECIMAL(147) TO QFDB-DDECIMAL-147.
           MOVE FDB-DNAME(148) TO QFDB-DNAME-148.
           MOVE FDB-DOFFSET(148) TO QFDB-DOFFSET-148.
           MOVE FDB-DDIGITS(148) TO QFDB-DDIGITS-148.
           MOVE FDB-DTYPE(148) TO QFDB-DTYPE-148.
           MOVE FDB-DDECIMAL(148) TO QFDB-DDECIMAL-148.
           MOVE FDB-DNAME(149) TO QFDB-DNAME-149.
           MOVE FDB-DOFFSET(149) TO QFDB-DOFFSET-149.
           MOVE FDB-DDIGITS(149) TO QFDB-DDIGITS-149.
           MOVE FDB-DTYPE(149) TO QFDB-DTYPE-149.
           MOVE FDB-DDECIMAL(149) TO QFDB-DDECIMAL-149.
           MOVE FDB-DNAME(150) TO QFDB-DNAME-150.
           MOVE FDB-DOFFSET(150) TO QFDB-DOFFSET-150.
           MOVE FDB-DDIGITS(150) TO QFDB-DDIGITS-150.
           MOVE FDB-DTYPE(150) TO QFDB-DTYPE-150.
           MOVE FDB-DDECIMAL(150) TO QFDB-DDECIMAL-150.
           MOVE FDB-DNAME(151) TO QFDB-DNAME-151.
           MOVE FDB-DOFFSET(151) TO QFDB-DOFFSET-151.
           MOVE FDB-DDIGITS(151) TO QFDB-DDIGITS-151.
           MOVE FDB-DTYPE(151) TO QFDB-DTYPE-151.
           MOVE FDB-DDECIMAL(151) TO QFDB-DDECIMAL-151.
           MOVE FDB-DNAME(152) TO QFDB-DNAME-152.
           MOVE FDB-DOFFSET(152) TO QFDB-DOFFSET-152.
           MOVE FDB-DDIGITS(152) TO QFDB-DDIGITS-152.
           MOVE FDB-DTYPE(152) TO QFDB-DTYPE-152.
           MOVE FDB-DDECIMAL(152) TO QFDB-DDECIMAL-152.
           MOVE FDB-DNAME(153) TO QFDB-DNAME-153.
           MOVE FDB-DOFFSET(153) TO QFDB-DOFFSET-153.
           MOVE FDB-DDIGITS(153) TO QFDB-DDIGITS-153.
           MOVE FDB-DTYPE(153) TO QFDB-DTYPE-153.
           MOVE FDB-DDECIMAL(153) TO QFDB-DDECIMAL-153.
           MOVE FDB-DNAME(154) TO QFDB-DNAME-154.
           MOVE FDB-DOFFSET(154) TO QFDB-DOFFSET-154.
           MOVE FDB-DDIGITS(154) TO QFDB-DDIGITS-154.
           MOVE FDB-DTYPE(154) TO QFDB-DTYPE-154.
           MOVE FDB-DDECIMAL(154) TO QFDB-DDECIMAL-154.
           MOVE FDB-DNAME(155) TO QFDB-DNAME-155.
           MOVE FDB-DOFFSET(155) TO QFDB-DOFFSET-155.
           MOVE FDB-DDIGITS(155) TO QFDB-DDIGITS-155.
           MOVE FDB-DTYPE(155) TO QFDB-DTYPE-155.
           MOVE FDB-DDECIMAL(155) TO QFDB-DDECIMAL-155.
           MOVE FDB-DNAME(156) TO QFDB-DNAME-156.
           MOVE FDB-DOFFSET(156) TO QFDB-DOFFSET-156.
           MOVE FDB-DDIGITS(156) TO QFDB-DDIGITS-156.
           MOVE FDB-DTYPE(156) TO QFDB-DTYPE-156.
           MOVE FDB-DDECIMAL(156) TO QFDB-DDECIMAL-156.
           MOVE FDB-DNAME(157) TO QFDB-DNAME-157.
           MOVE FDB-DOFFSET(157) TO QFDB-DOFFSET-157.
           MOVE FDB-DDIGITS(157) TO QFDB-DDIGITS-157.
           MOVE FDB-DTYPE(157) TO QFDB-DTYPE-157.
           MOVE FDB-DDECIMAL(157) TO QFDB-DDECIMAL-157.
           MOVE FDB-DNAME(158) TO QFDB-DNAME-158.
           MOVE FDB-DOFFSET(158) TO QFDB-DOFFSET-158.
           MOVE FDB-DDIGITS(158) TO QFDB-DDIGITS-158.
           MOVE FDB-DTYPE(158) TO QFDB-DTYPE-158.
           MOVE FDB-DDECIMAL(158) TO QFDB-DDECIMAL-158.
           MOVE FDB-DNAME(159) TO QFDB-DNAME-159.
           MOVE FDB-DOFFSET(159) TO QFDB-DOFFSET-159.
           MOVE FDB-DDIGITS(159) TO QFDB-DDIGITS-159.
           MOVE FDB-DTYPE(159) TO QFDB-DTYPE-159.
           MOVE FDB-DDECIMAL(159) TO QFDB-DDECIMAL-159.
           MOVE FDB-DNAME(160) TO QFDB-DNAME-160.
           MOVE FDB-DOFFSET(160) TO QFDB-DOFFSET-160.
           MOVE FDB-DDIGITS(160) TO QFDB-DDIGITS-160.
           MOVE FDB-DTYPE(160) TO QFDB-DTYPE-160.
           MOVE FDB-DDECIMAL(160) TO QFDB-DDECIMAL-160.
           MOVE FDB-DNAME(161) TO QFDB-DNAME-161.
           MOVE FDB-DOFFSET(161) TO QFDB-DOFFSET-161.
           MOVE FDB-DDIGITS(161) TO QFDB-DDIGITS-161.
           MOVE FDB-DTYPE(161) TO QFDB-DTYPE-161.
           MOVE FDB-DDECIMAL(161) TO QFDB-DDECIMAL-161.
           MOVE FDB-DNAME(162) TO QFDB-DNAME-162.
           MOVE FDB-DOFFSET(162) TO QFDB-DOFFSET-162.
           MOVE FDB-DDIGITS(162) TO QFDB-DDIGITS-162.
           MOVE FDB-DTYPE(162) TO QFDB-DTYPE-162.
           MOVE FDB-DDECIMAL(162) TO QFDB-DDECIMAL-162.
           MOVE FDB-DNAME(163) TO QFDB-DNAME-163.
           MOVE FDB-DOFFSET(163) TO QFDB-DOFFSET-163.
           MOVE FDB-DDIGITS(163) TO QFDB-DDIGITS-163.
           MOVE FDB-DTYPE(163) TO QFDB-DTYPE-163.
           MOVE FDB-DDECIMAL(163) TO QFDB-DDECIMAL-163.
           MOVE FDB-DNAME(164) TO QFDB-DNAME-164.
           MOVE FDB-DOFFSET(164) TO QFDB-DOFFSET-164.
           MOVE FDB-DDIGITS(164) TO QFDB-DDIGITS-164.
           MOVE FDB-DTYPE(164) TO QFDB-DTYPE-164.
           MOVE FDB-DDECIMAL(164) TO QFDB-DDECIMAL-164.
           MOVE FDB-DNAME(165) TO QFDB-DNAME-165.
           MOVE FDB-DOFFSET(165) TO QFDB-DOFFSET-165.
           MOVE FDB-DDIGITS(165) TO QFDB-DDIGITS-165.
           MOVE FDB-DTYPE(165) TO QFDB-DTYPE-165.
           MOVE FDB-DDECIMAL(165) TO QFDB-DDECIMAL-165.
           MOVE FDB-DNAME(166) TO QFDB-DNAME-166.
           MOVE FDB-DOFFSET(166) TO QFDB-DOFFSET-166.
           MOVE FDB-DDIGITS(166) TO QFDB-DDIGITS-166.
           MOVE FDB-DTYPE(166) TO QFDB-DTYPE-166.
           MOVE FDB-DDECIMAL(166) TO QFDB-DDECIMAL-166.
           MOVE FDB-DNAME(167) TO QFDB-DNAME-167.
           MOVE FDB-DOFFSET(167) TO QFDB-DOFFSET-167.
           MOVE FDB-DDIGITS(167) TO QFDB-DDIGITS-167.
           MOVE FDB-DTYPE(167) TO QFDB-DTYPE-167.
           MOVE FDB-DDECIMAL(167) TO QFDB-DDECIMAL-167.
           MOVE FDB-DNAME(168) TO QFDB-DNAME-168.
           MOVE FDB-DOFFSET(168) TO QFDB-DOFFSET-168.
           MOVE FDB-DDIGITS(168) TO QFDB-DDIGITS-168.
           MOVE FDB-DTYPE(168) TO QFDB-DTYPE-168.
           MOVE FDB-DDECIMAL(168) TO QFDB-DDECIMAL-168.
           MOVE FDB-DNAME(169) TO QFDB-DNAME-169.
           MOVE FDB-DOFFSET(169) TO QFDB-DOFFSET-169.
           MOVE FDB-DDIGITS(169) TO QFDB-DDIGITS-169.
           MOVE FDB-DTYPE(169) TO QFDB-DTYPE-169.
           MOVE FDB-DDECIMAL(169) TO QFDB-DDECIMAL-169.
           MOVE FDB-DNAME(170) TO QFDB-DNAME-170.
           MOVE FDB-DOFFSET(170) TO QFDB-DOFFSET-170.
           MOVE FDB-DDIGITS(170) TO QFDB-DDIGITS-170.
           MOVE FDB-DTYPE(170) TO QFDB-DTYPE-170.
           MOVE FDB-DDECIMAL(170) TO QFDB-DDECIMAL-170.
           MOVE FDB-DNAME(171) TO QFDB-DNAME-171.
           MOVE FDB-DOFFSET(171) TO QFDB-DOFFSET-171.
           MOVE FDB-DDIGITS(171) TO QFDB-DDIGITS-171.
           MOVE FDB-DTYPE(171) TO QFDB-DTYPE-171.
           MOVE FDB-DDECIMAL(171) TO QFDB-DDECIMAL-171.
           MOVE FDB-DNAME(172) TO QFDB-DNAME-172.
           MOVE FDB-DOFFSET(172) TO QFDB-DOFFSET-172.
           MOVE FDB-DDIGITS(172) TO QFDB-DDIGITS-172.
           MOVE FDB-DTYPE(172) TO QFDB-DTYPE-172.
           MOVE FDB-DDECIMAL(172) TO QFDB-DDECIMAL-172.
           MOVE FDB-DNAME(173) TO QFDB-DNAME-173.
           MOVE FDB-DOFFSET(173) TO QFDB-DOFFSET-173.
           MOVE FDB-DDIGITS(173) TO QFDB-DDIGITS-173.
           MOVE FDB-DTYPE(173) TO QFDB-DTYPE-173.
           MOVE FDB-DDECIMAL(173) TO QFDB-DDECIMAL-173.
           MOVE FDB-DNAME(174) TO QFDB-DNAME-174.
           MOVE FDB-DOFFSET(174) TO QFDB-DOFFSET-174.
           MOVE FDB-DDIGITS(174) TO QFDB-DDIGITS-174.
           MOVE FDB-DTYPE(174) TO QFDB-DTYPE-174.
           MOVE FDB-DDECIMAL(174) TO QFDB-DDECIMAL-174.
           MOVE FDB-DNAME(175) TO QFDB-DNAME-175.
           MOVE FDB-DOFFSET(175) TO QFDB-DOFFSET-175.
           MOVE FDB-DDIGITS(175) TO QFDB-DDIGITS-175.
           MOVE FDB-DTYPE(175) TO QFDB-DTYPE-175.
           MOVE FDB-DDECIMAL(175) TO QFDB-DDECIMAL-175.
           MOVE FDB-DNAME(176) TO QFDB-DNAME-176.
           MOVE FDB-DOFFSET(176) TO QFDB-DOFFSET-176.
           MOVE FDB-DDIGITS(176) TO QFDB-DDIGITS-176.
           MOVE FDB-DTYPE(176) TO QFDB-DTYPE-176.
           MOVE FDB-DDECIMAL(176) TO QFDB-DDECIMAL-176.
           MOVE FDB-DNAME(177) TO QFDB-DNAME-177.
           MOVE FDB-DOFFSET(177) TO QFDB-DOFFSET-177.
           MOVE FDB-DDIGITS(177) TO QFDB-DDIGITS-177.
           MOVE FDB-DTYPE(177) TO QFDB-DTYPE-177.
           MOVE FDB-DDECIMAL(177) TO QFDB-DDECIMAL-177.
           MOVE FDB-DNAME(178) TO QFDB-DNAME-178.
           MOVE FDB-DOFFSET(178) TO QFDB-DOFFSET-178.
           MOVE FDB-DDIGITS(178) TO QFDB-DDIGITS-178.
           MOVE FDB-DTYPE(178) TO QFDB-DTYPE-178.
           MOVE FDB-DDECIMAL(178) TO QFDB-DDECIMAL-178.
           MOVE FDB-DNAME(179) TO QFDB-DNAME-179.
           MOVE FDB-DOFFSET(179) TO QFDB-DOFFSET-179.
           MOVE FDB-DDIGITS(179) TO QFDB-DDIGITS-179.
           MOVE FDB-DTYPE(179) TO QFDB-DTYPE-179.
           MOVE FDB-DDECIMAL(179) TO QFDB-DDECIMAL-179.
           MOVE FDB-DNAME(180) TO QFDB-DNAME-180.
           MOVE FDB-DOFFSET(180) TO QFDB-DOFFSET-180.
           MOVE FDB-DDIGITS(180) TO QFDB-DDIGITS-180.
           MOVE FDB-DTYPE(180) TO QFDB-DTYPE-180.
           MOVE FDB-DDECIMAL(180) TO QFDB-DDECIMAL-180.
           MOVE FDB-DNAME(181) TO QFDB-DNAME-181.
           MOVE FDB-DOFFSET(181) TO QFDB-DOFFSET-181.
           MOVE FDB-DDIGITS(181) TO QFDB-DDIGITS-181.
           MOVE FDB-DTYPE(181) TO QFDB-DTYPE-181.
           MOVE FDB-DDECIMAL(181) TO QFDB-DDECIMAL-181.
           MOVE FDB-DNAME(182) TO QFDB-DNAME-182.
           MOVE FDB-DOFFSET(182) TO QFDB-DOFFSET-182.
           MOVE FDB-DDIGITS(182) TO QFDB-DDIGITS-182.
           MOVE FDB-DTYPE(182) TO QFDB-DTYPE-182.
           MOVE FDB-DDECIMAL(182) TO QFDB-DDECIMAL-182.
           MOVE FDB-DNAME(183) TO QFDB-DNAME-183.
           MOVE FDB-DOFFSET(183) TO QFDB-DOFFSET-183.
           MOVE FDB-DDIGITS(183) TO QFDB-DDIGITS-183.
           MOVE FDB-DTYPE(183) TO QFDB-DTYPE-183.
           MOVE FDB-DDECIMAL(183) TO QFDB-DDECIMAL-183.
           MOVE FDB-DNAME(184) TO QFDB-DNAME-184.
           MOVE FDB-DOFFSET(184) TO QFDB-DOFFSET-184.
           MOVE FDB-DDIGITS(184) TO QFDB-DDIGITS-184.
           MOVE FDB-DTYPE(184) TO QFDB-DTYPE-184.
           MOVE FDB-DDECIMAL(184) TO QFDB-DDECIMAL-184.
           MOVE FDB-DNAME(185) TO QFDB-DNAME-185.
           MOVE FDB-DOFFSET(185) TO QFDB-DOFFSET-185.
           MOVE FDB-DDIGITS(185) TO QFDB-DDIGITS-185.
           MOVE FDB-DTYPE(185) TO QFDB-DTYPE-185.
           MOVE FDB-DDECIMAL(185) TO QFDB-DDECIMAL-185.
           MOVE FDB-DNAME(186) TO QFDB-DNAME-186.
           MOVE FDB-DOFFSET(186) TO QFDB-DOFFSET-186.
           MOVE FDB-DDIGITS(186) TO QFDB-DDIGITS-186.
           MOVE FDB-DTYPE(186) TO QFDB-DTYPE-186.
           MOVE FDB-DDECIMAL(186) TO QFDB-DDECIMAL-186.
           MOVE FDB-DNAME(187) TO QFDB-DNAME-187.
           MOVE FDB-DOFFSET(187) TO QFDB-DOFFSET-187.
           MOVE FDB-DDIGITS(187) TO QFDB-DDIGITS-187.
           MOVE FDB-DTYPE(187) TO QFDB-DTYPE-187.
           MOVE FDB-DDECIMAL(187) TO QFDB-DDECIMAL-187.
           MOVE FDB-DNAME(188) TO QFDB-DNAME-188.
           MOVE FDB-DOFFSET(188) TO QFDB-DOFFSET-188.
           MOVE FDB-DDIGITS(188) TO QFDB-DDIGITS-188.
           MOVE FDB-DTYPE(188) TO QFDB-DTYPE-188.
           MOVE FDB-DDECIMAL(188) TO QFDB-DDECIMAL-188.
           MOVE FDB-DNAME(189) TO QFDB-DNAME-189.
           MOVE FDB-DOFFSET(189) TO QFDB-DOFFSET-189.
           MOVE FDB-DDIGITS(189) TO QFDB-DDIGITS-189.
           MOVE FDB-DTYPE(189) TO QFDB-DTYPE-189.
           MOVE FDB-DDECIMAL(189) TO QFDB-DDECIMAL-189.
           MOVE FDB-DNAME(190) TO QFDB-DNAME-190.
           MOVE FDB-DOFFSET(190) TO QFDB-DOFFSET-190.
           MOVE FDB-DDIGITS(190) TO QFDB-DDIGITS-190.
           MOVE FDB-DTYPE(190) TO QFDB-DTYPE-190.
           MOVE FDB-DDECIMAL(190) TO QFDB-DDECIMAL-190.
           MOVE FDB-DNAME(191) TO QFDB-DNAME-191.
           MOVE FDB-DOFFSET(191) TO QFDB-DOFFSET-191.
           MOVE FDB-DDIGITS(191) TO QFDB-DDIGITS-191.
           MOVE FDB-DTYPE(191) TO QFDB-DTYPE-191.
           MOVE FDB-DDECIMAL(191) TO QFDB-DDECIMAL-191.
           MOVE FDB-DNAME(192) TO QFDB-DNAME-192.
           MOVE FDB-DOFFSET(192) TO QFDB-DOFFSET-192.
           MOVE FDB-DDIGITS(192) TO QFDB-DDIGITS-192.
           MOVE FDB-DTYPE(192) TO QFDB-DTYPE-192.
           MOVE FDB-DDECIMAL(192) TO QFDB-DDECIMAL-192.
           MOVE FDB-DNAME(193) TO QFDB-DNAME-193.
           MOVE FDB-DOFFSET(193) TO QFDB-DOFFSET-193.
           MOVE FDB-DDIGITS(193) TO QFDB-DDIGITS-193.
           MOVE FDB-DTYPE(193) TO QFDB-DTYPE-193.
           MOVE FDB-DDECIMAL(193) TO QFDB-DDECIMAL-193.
           MOVE FDB-DNAME(194) TO QFDB-DNAME-194.
           MOVE FDB-DOFFSET(194) TO QFDB-DOFFSET-194.
           MOVE FDB-DDIGITS(194) TO QFDB-DDIGITS-194.
           MOVE FDB-DTYPE(194) TO QFDB-DTYPE-194.
           MOVE FDB-DDECIMAL(194) TO QFDB-DDECIMAL-194.
           MOVE FDB-DNAME(195) TO QFDB-DNAME-195.
           MOVE FDB-DOFFSET(195) TO QFDB-DOFFSET-195.
           MOVE FDB-DDIGITS(195) TO QFDB-DDIGITS-195.
           MOVE FDB-DTYPE(195) TO QFDB-DTYPE-195.
           MOVE FDB-DDECIMAL(195) TO QFDB-DDECIMAL-195.
           MOVE FDB-DNAME(196) TO QFDB-DNAME-196.
           MOVE FDB-DOFFSET(196) TO QFDB-DOFFSET-196.
           MOVE FDB-DDIGITS(196) TO QFDB-DDIGITS-196.
           MOVE FDB-DTYPE(196) TO QFDB-DTYPE-196.
           MOVE FDB-DDECIMAL(196) TO QFDB-DDECIMAL-196.
           MOVE FDB-DNAME(197) TO QFDB-DNAME-197.
           MOVE FDB-DOFFSET(197) TO QFDB-DOFFSET-197.
           MOVE FDB-DDIGITS(197) TO QFDB-DDIGITS-197.
           MOVE FDB-DTYPE(197) TO QFDB-DTYPE-197.
           MOVE FDB-DDECIMAL(197) TO QFDB-DDECIMAL-197.
           MOVE FDB-DNAME(198) TO QFDB-DNAME-198.
           MOVE FDB-DOFFSET(198) TO QFDB-DOFFSET-198.
           MOVE FDB-DDIGITS(198) TO QFDB-DDIGITS-198.
           MOVE FDB-DTYPE(198) TO QFDB-DTYPE-198.
           MOVE FDB-DDECIMAL(198) TO QFDB-DDECIMAL-198.
           MOVE FDB-DNAME(199) TO QFDB-DNAME-199.
           MOVE FDB-DOFFSET(199) TO QFDB-DOFFSET-199.
           MOVE FDB-DDIGITS(199) TO QFDB-DDIGITS-199.
           MOVE FDB-DTYPE(199) TO QFDB-DTYPE-199.
           MOVE FDB-DDECIMAL(199) TO QFDB-DDECIMAL-199.
           MOVE FDB-DNAME(200) TO QFDB-DNAME-200.
           MOVE FDB-DOFFSET(200) TO QFDB-DOFFSET-200.
           MOVE FDB-DDIGITS(200) TO QFDB-DDIGITS-200.
           MOVE FDB-DTYPE(200) TO QFDB-DTYPE-200.
           MOVE FDB-DDECIMAL(200) TO QFDB-DDECIMAL-200.
           MOVE FDB-CALLID(1) TO QFDB-CALLID-1.
           MOVE FDB-KEYLOC(1) TO QFDB-KEYLOC-1.
           MOVE FDB-CALLID(2) TO QFDB-CALLID-2.
           MOVE FDB-KEYLOC(2) TO QFDB-KEYLOC-2.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPFDB1RN
       LOAD-FDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FDB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FDB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FDB-ALL-MACHINE
              MOVE FDB-ALL-PATH            TO FDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FDB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO FDB-ALL-MACHINE
              MOVE FDB-ALL-PATH            TO FDB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-FDBFILE NOT = "S" )
              MOVE FDB-PATH-BEG             TO FDB-SELECT-PATH-BEG
              MOVE FDB-PATH-FILE            TO FDB-SELECT-PATH-FILE
           ELSE
              MOVE FDB-PATH-SQL             TO FDB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-FDB1-FILE.
           PERFORM LOAD-FDB1-FILE.
           PERFORM OPEN-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-FDB1-FILE.
           PERFORM START-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.

           IF ( FDB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FDB1-FILE.

           IF ( QFDB1-WEND-DBNO = ZEROES    ) OR
              ( QFDB1-WEND-DBNO NOT NUMERIC )
              MOVE FDB-DBNO       TO QFDB1-WBEG-DBNO
              MOVE ALL "9"        TO QFDB1-WEND-DBNO.

           EXEC SQL
            DECLARE FDB1_CURSOR CURSOR FOR
            SELECT FDBFILE.FDB_DBNO,
                   ISNULL(CAST(FDBFILE.FDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FDBFILE.FDB_DNAME_1,''),
                   FDBFILE.FDB_DOFFSET_1,
                   FDBFILE.FDB_DDIGITS_1,
                   FDBFILE.FDB_DTYPE_1,
                   FDBFILE.FDB_DDECIMAL_1,
                   ISNULL(FDBFILE.FDB_DNAME_2,''),
                   FDBFILE.FDB_DOFFSET_2,
                   FDBFILE.FDB_DDIGITS_2,
                   FDBFILE.FDB_DTYPE_2,
                   FDBFILE.FDB_DDECIMAL_2,
                   ISNULL(FDBFILE.FDB_DNAME_3,''),
                   FDBFILE.FDB_DOFFSET_3,
                   FDBFILE.FDB_DDIGITS_3,
                   FDBFILE.FDB_DTYPE_3,
                   FDBFILE.FDB_DDECIMAL_3,
                   ISNULL(FDBFILE.FDB_DNAME_4,''),
                   FDBFILE.FDB_DOFFSET_4,
                   FDBFILE.FDB_DDIGITS_4,
                   FDBFILE.FDB_DTYPE_4,
                   FDBFILE.FDB_DDECIMAL_4,
                   ISNULL(FDBFILE.FDB_DNAME_5,''),
                   FDBFILE.FDB_DOFFSET_5,
                   FDBFILE.FDB_DDIGITS_5,
                   FDBFILE.FDB_DTYPE_5,
                   FDBFILE.FDB_DDECIMAL_5,
                   ISNULL(FDBFILE.FDB_DNAME_6,''),
                   FDBFILE.FDB_DOFFSET_6,
                   FDBFILE.FDB_DDIGITS_6,
                   FDBFILE.FDB_DTYPE_6,
                   FDBFILE.FDB_DDECIMAL_6,
                   ISNULL(FDBFILE.FDB_DNAME_7,''),
                   FDBFILE.FDB_DOFFSET_7,
                   FDBFILE.FDB_DDIGITS_7,
                   FDBFILE.FDB_DTYPE_7,
                   FDBFILE.FDB_DDECIMAL_7,
                   ISNULL(FDBFILE.FDB_DNAME_8,''),
                   FDBFILE.FDB_DOFFSET_8,
                   FDBFILE.FDB_DDIGITS_8,
                   FDBFILE.FDB_DTYPE_8,
                   FDBFILE.FDB_DDECIMAL_8,
                   ISNULL(FDBFILE.FDB_DNAME_9,''),
                   FDBFILE.FDB_DOFFSET_9,
                   FDBFILE.FDB_DDIGITS_9,
                   FDBFILE.FDB_DTYPE_9,
                   FDBFILE.FDB_DDECIMAL_9,
                   ISNULL(FDBFILE.FDB_DNAME_10,''),
                   FDBFILE.FDB_DOFFSET_10,
                   FDBFILE.FDB_DDIGITS_10,
                   FDBFILE.FDB_DTYPE_10,
                   FDBFILE.FDB_DDECIMAL_10,
                   ISNULL(FDBFILE.FDB_DNAME_11,''),
                   FDBFILE.FDB_DOFFSET_11,
                   FDBFILE.FDB_DDIGITS_11,
                   FDBFILE.FDB_DTYPE_11,
                   FDBFILE.FDB_DDECIMAL_11,
                   ISNULL(FDBFILE.FDB_DNAME_12,''),
                   FDBFILE.FDB_DOFFSET_12,
                   FDBFILE.FDB_DDIGITS_12,
                   FDBFILE.FDB_DTYPE_12,
                   FDBFILE.FDB_DDECIMAL_12,
                   ISNULL(FDBFILE.FDB_DNAME_13,''),
                   FDBFILE.FDB_DOFFSET_13,
                   FDBFILE.FDB_DDIGITS_13,
                   FDBFILE.FDB_DTYPE_13,
                   FDBFILE.FDB_DDECIMAL_13,
                   ISNULL(FDBFILE.FDB_DNAME_14,''),
                   FDBFILE.FDB_DOFFSET_14,
                   FDBFILE.FDB_DDIGITS_14,
                   FDBFILE.FDB_DTYPE_14,
                   FDBFILE.FDB_DDECIMAL_14,
                   ISNULL(FDBFILE.FDB_DNAME_15,''),
                   FDBFILE.FDB_DOFFSET_15,
                   FDBFILE.FDB_DDIGITS_15,
                   FDBFILE.FDB_DTYPE_15,
                   FDBFILE.FDB_DDECIMAL_15,
                   ISNULL(FDBFILE.FDB_DNAME_16,''),
                   FDBFILE.FDB_DOFFSET_16,
                   FDBFILE.FDB_DDIGITS_16,
                   FDBFILE.FDB_DTYPE_16,
                   FDBFILE.FDB_DDECIMAL_16,
                   ISNULL(FDBFILE.FDB_DNAME_17,''),
                   FDBFILE.FDB_DOFFSET_17,
                   FDBFILE.FDB_DDIGITS_17,
                   FDBFILE.FDB_DTYPE_17,
                   FDBFILE.FDB_DDECIMAL_17,
                   ISNULL(FDBFILE.FDB_DNAME_18,''),
                   FDBFILE.FDB_DOFFSET_18,
                   FDBFILE.FDB_DDIGITS_18,
                   FDBFILE.FDB_DTYPE_18,
                   FDBFILE.FDB_DDECIMAL_18,
                   ISNULL(FDBFILE.FDB_DNAME_19,''),
                   FDBFILE.FDB_DOFFSET_19,
                   FDBFILE.FDB_DDIGITS_19,
                   FDBFILE.FDB_DTYPE_19,
                   FDBFILE.FDB_DDECIMAL_19,
                   ISNULL(FDBFILE.FDB_DNAME_20,''),
                   FDBFILE.FDB_DOFFSET_20,
                   FDBFILE.FDB_DDIGITS_20,
                   FDBFILE.FDB_DTYPE_20,
                   FDBFILE.FDB_DDECIMAL_20,
                   ISNULL(FDBFILE.FDB_DNAME_21,''),
                   FDBFILE.FDB_DOFFSET_21,
                   FDBFILE.FDB_DDIGITS_21,
                   FDBFILE.FDB_DTYPE_21,
                   FDBFILE.FDB_DDECIMAL_21,
                   ISNULL(FDBFILE.FDB_DNAME_22,''),
                   FDBFILE.FDB_DOFFSET_22,
                   FDBFILE.FDB_DDIGITS_22,
                   FDBFILE.FDB_DTYPE_22,
                   FDBFILE.FDB_DDECIMAL_22,
                   ISNULL(FDBFILE.FDB_DNAME_23,''),
                   FDBFILE.FDB_DOFFSET_23,
                   FDBFILE.FDB_DDIGITS_23,
                   FDBFILE.FDB_DTYPE_23,
                   FDBFILE.FDB_DDECIMAL_23,
                   ISNULL(FDBFILE.FDB_DNAME_24,''),
                   FDBFILE.FDB_DOFFSET_24,
                   FDBFILE.FDB_DDIGITS_24,
                   FDBFILE.FDB_DTYPE_24,
                   FDBFILE.FDB_DDECIMAL_24,
                   ISNULL(FDBFILE.FDB_DNAME_25,''),
                   FDBFILE.FDB_DOFFSET_25,
                   FDBFILE.FDB_DDIGITS_25,
                   FDBFILE.FDB_DTYPE_25,
                   FDBFILE.FDB_DDECIMAL_25,
                   ISNULL(FDBFILE.FDB_DNAME_26,''),
                   FDBFILE.FDB_DOFFSET_26,
                   FDBFILE.FDB_DDIGITS_26,
                   FDBFILE.FDB_DTYPE_26,
                   FDBFILE.FDB_DDECIMAL_26,
                   ISNULL(FDBFILE.FDB_DNAME_27,''),
                   FDBFILE.FDB_DOFFSET_27,
                   FDBFILE.FDB_DDIGITS_27,
                   FDBFILE.FDB_DTYPE_27,
                   FDBFILE.FDB_DDECIMAL_27,
                   ISNULL(FDBFILE.FDB_DNAME_28,''),
                   FDBFILE.FDB_DOFFSET_28,
                   FDBFILE.FDB_DDIGITS_28,
                   FDBFILE.FDB_DTYPE_28,
                   FDBFILE.FDB_DDECIMAL_28,
                   ISNULL(FDBFILE.FDB_DNAME_29,''),
                   FDBFILE.FDB_DOFFSET_29,
                   FDBFILE.FDB_DDIGITS_29,
                   FDBFILE.FDB_DTYPE_29,
                   FDBFILE.FDB_DDECIMAL_29,
                   ISNULL(FDBFILE.FDB_DNAME_30,''),
                   FDBFILE.FDB_DOFFSET_30,
                   FDBFILE.FDB_DDIGITS_30,
                   FDBFILE.FDB_DTYPE_30,
                   FDBFILE.FDB_DDECIMAL_30,
                   ISNULL(FDBFILE.FDB_DNAME_31,''),
                   FDBFILE.FDB_DOFFSET_31,
                   FDBFILE.FDB_DDIGITS_31,
                   FDBFILE.FDB_DTYPE_31,
                   FDBFILE.FDB_DDECIMAL_31,
                   ISNULL(FDBFILE.FDB_DNAME_32,''),
                   FDBFILE.FDB_DOFFSET_32,
                   FDBFILE.FDB_DDIGITS_32,
                   FDBFILE.FDB_DTYPE_32,
                   FDBFILE.FDB_DDECIMAL_32,
                   ISNULL(FDBFILE.FDB_DNAME_33,''),
                   FDBFILE.FDB_DOFFSET_33,
                   FDBFILE.FDB_DDIGITS_33,
                   FDBFILE.FDB_DTYPE_33,
                   FDBFILE.FDB_DDECIMAL_33,
                   ISNULL(FDBFILE.FDB_DNAME_34,''),
                   FDBFILE.FDB_DOFFSET_34,
                   FDBFILE.FDB_DDIGITS_34,
                   FDBFILE.FDB_DTYPE_34,
                   FDBFILE.FDB_DDECIMAL_34,
                   ISNULL(FDBFILE.FDB_DNAME_35,''),
                   FDBFILE.FDB_DOFFSET_35,
                   FDBFILE.FDB_DDIGITS_35,
                   FDBFILE.FDB_DTYPE_35,
                   FDBFILE.FDB_DDECIMAL_35,
                   ISNULL(FDBFILE.FDB_DNAME_36,''),
                   FDBFILE.FDB_DOFFSET_36,
                   FDBFILE.FDB_DDIGITS_36,
                   FDBFILE.FDB_DTYPE_36,
                   FDBFILE.FDB_DDECIMAL_36,
                   ISNULL(FDBFILE.FDB_DNAME_37,''),
                   FDBFILE.FDB_DOFFSET_37,
                   FDBFILE.FDB_DDIGITS_37,
                   FDBFILE.FDB_DTYPE_37,
                   FDBFILE.FDB_DDECIMAL_37,
                   ISNULL(FDBFILE.FDB_DNAME_38,''),
                   FDBFILE.FDB_DOFFSET_38,
                   FDBFILE.FDB_DDIGITS_38,
                   FDBFILE.FDB_DTYPE_38,
                   FDBFILE.FDB_DDECIMAL_38,
                   ISNULL(FDBFILE.FDB_DNAME_39,''),
                   FDBFILE.FDB_DOFFSET_39,
                   FDBFILE.FDB_DDIGITS_39,
                   FDBFILE.FDB_DTYPE_39,
                   FDBFILE.FDB_DDECIMAL_39,
                   ISNULL(FDBFILE.FDB_DNAME_40,''),
                   FDBFILE.FDB_DOFFSET_40,
                   FDBFILE.FDB_DDIGITS_40,
                   FDBFILE.FDB_DTYPE_40,
                   FDBFILE.FDB_DDECIMAL_40,
                   ISNULL(FDBFILE.FDB_DNAME_41,''),
                   FDBFILE.FDB_DOFFSET_41,
                   FDBFILE.FDB_DDIGITS_41,
                   FDBFILE.FDB_DTYPE_41,
                   FDBFILE.FDB_DDECIMAL_41,
                   ISNULL(FDBFILE.FDB_DNAME_42,''),
                   FDBFILE.FDB_DOFFSET_42,
                   FDBFILE.FDB_DDIGITS_42,
                   FDBFILE.FDB_DTYPE_42,
                   FDBFILE.FDB_DDECIMAL_42,
                   ISNULL(FDBFILE.FDB_DNAME_43,''),
                   FDBFILE.FDB_DOFFSET_43,
                   FDBFILE.FDB_DDIGITS_43,
                   FDBFILE.FDB_DTYPE_43,
                   FDBFILE.FDB_DDECIMAL_43,
                   ISNULL(FDBFILE.FDB_DNAME_44,''),
                   FDBFILE.FDB_DOFFSET_44,
                   FDBFILE.FDB_DDIGITS_44,
                   FDBFILE.FDB_DTYPE_44,
                   FDBFILE.FDB_DDECIMAL_44,
                   ISNULL(FDBFILE.FDB_DNAME_45,''),
                   FDBFILE.FDB_DOFFSET_45,
                   FDBFILE.FDB_DDIGITS_45,
                   FDBFILE.FDB_DTYPE_45,
                   FDBFILE.FDB_DDECIMAL_45,
                   ISNULL(FDBFILE.FDB_DNAME_46,''),
                   FDBFILE.FDB_DOFFSET_46,
                   FDBFILE.FDB_DDIGITS_46,
                   FDBFILE.FDB_DTYPE_46,
                   FDBFILE.FDB_DDECIMAL_46,
                   ISNULL(FDBFILE.FDB_DNAME_47,''),
                   FDBFILE.FDB_DOFFSET_47,
                   FDBFILE.FDB_DDIGITS_47,
                   FDBFILE.FDB_DTYPE_47,
                   FDBFILE.FDB_DDECIMAL_47,
                   ISNULL(FDBFILE.FDB_DNAME_48,''),
                   FDBFILE.FDB_DOFFSET_48,
                   FDBFILE.FDB_DDIGITS_48,
                   FDBFILE.FDB_DTYPE_48,
                   FDBFILE.FDB_DDECIMAL_48,
                   ISNULL(FDBFILE.FDB_DNAME_49,''),
                   FDBFILE.FDB_DOFFSET_49,
                   FDBFILE.FDB_DDIGITS_49,
                   FDBFILE.FDB_DTYPE_49,
                   FDBFILE.FDB_DDECIMAL_49,
                   ISNULL(FDBFILE.FDB_DNAME_50,''),
                   FDBFILE.FDB_DOFFSET_50,
                   FDBFILE.FDB_DDIGITS_50,
                   FDBFILE.FDB_DTYPE_50,
                   FDBFILE.FDB_DDECIMAL_50,
                   ISNULL(FDBFILE.FDB_DNAME_51,''),
                   FDBFILE.FDB_DOFFSET_51,
                   FDBFILE.FDB_DDIGITS_51,
                   FDBFILE.FDB_DTYPE_51,
                   FDBFILE.FDB_DDECIMAL_51,
                   ISNULL(FDBFILE.FDB_DNAME_52,''),
                   FDBFILE.FDB_DOFFSET_52,
                   FDBFILE.FDB_DDIGITS_52,
                   FDBFILE.FDB_DTYPE_52,
                   FDBFILE.FDB_DDECIMAL_52,
                   ISNULL(FDBFILE.FDB_DNAME_53,''),
                   FDBFILE.FDB_DOFFSET_53,
                   FDBFILE.FDB_DDIGITS_53,
                   FDBFILE.FDB_DTYPE_53,
                   FDBFILE.FDB_DDECIMAL_53,
                   ISNULL(FDBFILE.FDB_DNAME_54,''),
                   FDBFILE.FDB_DOFFSET_54,
                   FDBFILE.FDB_DDIGITS_54,
                   FDBFILE.FDB_DTYPE_54,
                   FDBFILE.FDB_DDECIMAL_54,
                   ISNULL(FDBFILE.FDB_DNAME_55,''),
                   FDBFILE.FDB_DOFFSET_55,
                   FDBFILE.FDB_DDIGITS_55,
                   FDBFILE.FDB_DTYPE_55,
                   FDBFILE.FDB_DDECIMAL_55,
                   ISNULL(FDBFILE.FDB_DNAME_56,''),
                   FDBFILE.FDB_DOFFSET_56,
                   FDBFILE.FDB_DDIGITS_56,
                   FDBFILE.FDB_DTYPE_56,
                   FDBFILE.FDB_DDECIMAL_56,
                   ISNULL(FDBFILE.FDB_DNAME_57,''),
                   FDBFILE.FDB_DOFFSET_57,
                   FDBFILE.FDB_DDIGITS_57,
                   FDBFILE.FDB_DTYPE_57,
                   FDBFILE.FDB_DDECIMAL_57,
                   ISNULL(FDBFILE.FDB_DNAME_58,''),
                   FDBFILE.FDB_DOFFSET_58,
                   FDBFILE.FDB_DDIGITS_58,
                   FDBFILE.FDB_DTYPE_58,
                   FDBFILE.FDB_DDECIMAL_58,
                   ISNULL(FDBFILE.FDB_DNAME_59,''),
                   FDBFILE.FDB_DOFFSET_59,
                   FDBFILE.FDB_DDIGITS_59,
                   FDBFILE.FDB_DTYPE_59,
                   FDBFILE.FDB_DDECIMAL_59,
                   ISNULL(FDBFILE.FDB_DNAME_60,''),
                   FDBFILE.FDB_DOFFSET_60,
                   FDBFILE.FDB_DDIGITS_60,
                   FDBFILE.FDB_DTYPE_60,
                   FDBFILE.FDB_DDECIMAL_60,
                   ISNULL(FDBFILE.FDB_DNAME_61,''),
                   FDBFILE.FDB_DOFFSET_61,
                   FDBFILE.FDB_DDIGITS_61,
                   FDBFILE.FDB_DTYPE_61,
                   FDBFILE.FDB_DDECIMAL_61,
                   ISNULL(FDBFILE.FDB_DNAME_62,''),
                   FDBFILE.FDB_DOFFSET_62,
                   FDBFILE.FDB_DDIGITS_62,
                   FDBFILE.FDB_DTYPE_62,
                   FDBFILE.FDB_DDECIMAL_62,
                   ISNULL(FDBFILE.FDB_DNAME_63,''),
                   FDBFILE.FDB_DOFFSET_63,
                   FDBFILE.FDB_DDIGITS_63,
                   FDBFILE.FDB_DTYPE_63,
                   FDBFILE.FDB_DDECIMAL_63,
                   ISNULL(FDBFILE.FDB_DNAME_64,''),
                   FDBFILE.FDB_DOFFSET_64,
                   FDBFILE.FDB_DDIGITS_64,
                   FDBFILE.FDB_DTYPE_64,
                   FDBFILE.FDB_DDECIMAL_64,
                   ISNULL(FDBFILE.FDB_DNAME_65,''),
                   FDBFILE.FDB_DOFFSET_65,
                   FDBFILE.FDB_DDIGITS_65,
                   FDBFILE.FDB_DTYPE_65,
                   FDBFILE.FDB_DDECIMAL_65,
                   ISNULL(FDBFILE.FDB_DNAME_66,''),
                   FDBFILE.FDB_DOFFSET_66,
                   FDBFILE.FDB_DDIGITS_66,
                   FDBFILE.FDB_DTYPE_66,
                   FDBFILE.FDB_DDECIMAL_66,
                   ISNULL(FDBFILE.FDB_DNAME_67,''),
                   FDBFILE.FDB_DOFFSET_67,
                   FDBFILE.FDB_DDIGITS_67,
                   FDBFILE.FDB_DTYPE_67,
                   FDBFILE.FDB_DDECIMAL_67,
                   ISNULL(FDBFILE.FDB_DNAME_68,''),
                   FDBFILE.FDB_DOFFSET_68,
                   FDBFILE.FDB_DDIGITS_68,
                   FDBFILE.FDB_DTYPE_68,
                   FDBFILE.FDB_DDECIMAL_68,
                   ISNULL(FDBFILE.FDB_DNAME_69,''),
                   FDBFILE.FDB_DOFFSET_69,
                   FDBFILE.FDB_DDIGITS_69,
                   FDBFILE.FDB_DTYPE_69,
                   FDBFILE.FDB_DDECIMAL_69,
                   ISNULL(FDBFILE.FDB_DNAME_70,''),
                   FDBFILE.FDB_DOFFSET_70,
                   FDBFILE.FDB_DDIGITS_70,
                   FDBFILE.FDB_DTYPE_70,
                   FDBFILE.FDB_DDECIMAL_70,
                   ISNULL(FDBFILE.FDB_DNAME_71,''),
                   FDBFILE.FDB_DOFFSET_71,
                   FDBFILE.FDB_DDIGITS_71,
                   FDBFILE.FDB_DTYPE_71,
                   FDBFILE.FDB_DDECIMAL_71,
                   ISNULL(FDBFILE.FDB_DNAME_72,''),
                   FDBFILE.FDB_DOFFSET_72,
                   FDBFILE.FDB_DDIGITS_72,
                   FDBFILE.FDB_DTYPE_72,
                   FDBFILE.FDB_DDECIMAL_72,
                   ISNULL(FDBFILE.FDB_DNAME_73,''),
                   FDBFILE.FDB_DOFFSET_73,
                   FDBFILE.FDB_DDIGITS_73,
                   FDBFILE.FDB_DTYPE_73,
                   FDBFILE.FDB_DDECIMAL_73,
                   ISNULL(FDBFILE.FDB_DNAME_74,''),
                   FDBFILE.FDB_DOFFSET_74,
                   FDBFILE.FDB_DDIGITS_74,
                   FDBFILE.FDB_DTYPE_74,
                   FDBFILE.FDB_DDECIMAL_74,
                   ISNULL(FDBFILE.FDB_DNAME_75,''),
                   FDBFILE.FDB_DOFFSET_75,
                   FDBFILE.FDB_DDIGITS_75,
                   FDBFILE.FDB_DTYPE_75,
                   FDBFILE.FDB_DDECIMAL_75,
                   ISNULL(FDBFILE.FDB_DNAME_76,''),
                   FDBFILE.FDB_DOFFSET_76,
                   FDBFILE.FDB_DDIGITS_76,
                   FDBFILE.FDB_DTYPE_76,
                   FDBFILE.FDB_DDECIMAL_76,
                   ISNULL(FDBFILE.FDB_DNAME_77,''),
                   FDBFILE.FDB_DOFFSET_77,
                   FDBFILE.FDB_DDIGITS_77,
                   FDBFILE.FDB_DTYPE_77,
                   FDBFILE.FDB_DDECIMAL_77,
                   ISNULL(FDBFILE.FDB_DNAME_78,''),
                   FDBFILE.FDB_DOFFSET_78,
                   FDBFILE.FDB_DDIGITS_78,
                   FDBFILE.FDB_DTYPE_78,
                   FDBFILE.FDB_DDECIMAL_78,
                   ISNULL(FDBFILE.FDB_DNAME_79,''),
                   FDBFILE.FDB_DOFFSET_79,
                   FDBFILE.FDB_DDIGITS_79,
                   FDBFILE.FDB_DTYPE_79,
                   FDBFILE.FDB_DDECIMAL_79,
                   ISNULL(FDBFILE.FDB_DNAME_80,''),
                   FDBFILE.FDB_DOFFSET_80,
                   FDBFILE.FDB_DDIGITS_80,
                   FDBFILE.FDB_DTYPE_80,
                   FDBFILE.FDB_DDECIMAL_80,
                   ISNULL(FDBFILE.FDB_DNAME_81,''),
                   FDBFILE.FDB_DOFFSET_81,
                   FDBFILE.FDB_DDIGITS_81,
                   FDBFILE.FDB_DTYPE_81,
                   FDBFILE.FDB_DDECIMAL_81,
                   ISNULL(FDBFILE.FDB_DNAME_82,''),
                   FDBFILE.FDB_DOFFSET_82,
                   FDBFILE.FDB_DDIGITS_82,
                   FDBFILE.FDB_DTYPE_82,
                   FDBFILE.FDB_DDECIMAL_82,
                   ISNULL(FDBFILE.FDB_DNAME_83,''),
                   FDBFILE.FDB_DOFFSET_83,
                   FDBFILE.FDB_DDIGITS_83,
                   FDBFILE.FDB_DTYPE_83,
                   FDBFILE.FDB_DDECIMAL_83,
                   ISNULL(FDBFILE.FDB_DNAME_84,''),
                   FDBFILE.FDB_DOFFSET_84,
                   FDBFILE.FDB_DDIGITS_84,
                   FDBFILE.FDB_DTYPE_84,
                   FDBFILE.FDB_DDECIMAL_84,
                   ISNULL(FDBFILE.FDB_DNAME_85,''),
                   FDBFILE.FDB_DOFFSET_85,
                   FDBFILE.FDB_DDIGITS_85,
                   FDBFILE.FDB_DTYPE_85,
                   FDBFILE.FDB_DDECIMAL_85,
                   ISNULL(FDBFILE.FDB_DNAME_86,''),
                   FDBFILE.FDB_DOFFSET_86,
                   FDBFILE.FDB_DDIGITS_86,
                   FDBFILE.FDB_DTYPE_86,
                   FDBFILE.FDB_DDECIMAL_86,
                   ISNULL(FDBFILE.FDB_DNAME_87,''),
                   FDBFILE.FDB_DOFFSET_87,
                   FDBFILE.FDB_DDIGITS_87,
                   FDBFILE.FDB_DTYPE_87,
                   FDBFILE.FDB_DDECIMAL_87,
                   ISNULL(FDBFILE.FDB_DNAME_88,''),
                   FDBFILE.FDB_DOFFSET_88,
                   FDBFILE.FDB_DDIGITS_88,
                   FDBFILE.FDB_DTYPE_88,
                   FDBFILE.FDB_DDECIMAL_88,
                   ISNULL(FDBFILE.FDB_DNAME_89,''),
                   FDBFILE.FDB_DOFFSET_89,
                   FDBFILE.FDB_DDIGITS_89,
                   FDBFILE.FDB_DTYPE_89,
                   FDBFILE.FDB_DDECIMAL_89,
                   ISNULL(FDBFILE.FDB_DNAME_90,''),
                   FDBFILE.FDB_DOFFSET_90,
                   FDBFILE.FDB_DDIGITS_90,
                   FDBFILE.FDB_DTYPE_90,
                   FDBFILE.FDB_DDECIMAL_90,
                   ISNULL(FDBFILE.FDB_DNAME_91,''),
                   FDBFILE.FDB_DOFFSET_91,
                   FDBFILE.FDB_DDIGITS_91,
                   FDBFILE.FDB_DTYPE_91,
                   FDBFILE.FDB_DDECIMAL_91,
                   ISNULL(FDBFILE.FDB_DNAME_92,''),
                   FDBFILE.FDB_DOFFSET_92,
                   FDBFILE.FDB_DDIGITS_92,
                   FDBFILE.FDB_DTYPE_92,
                   FDBFILE.FDB_DDECIMAL_92,
                   ISNULL(FDBFILE.FDB_DNAME_93,''),
                   FDBFILE.FDB_DOFFSET_93,
                   FDBFILE.FDB_DDIGITS_93,
                   FDBFILE.FDB_DTYPE_93,
                   FDBFILE.FDB_DDECIMAL_93,
                   ISNULL(FDBFILE.FDB_DNAME_94,''),
                   FDBFILE.FDB_DOFFSET_94,
                   FDBFILE.FDB_DDIGITS_94,
                   FDBFILE.FDB_DTYPE_94,
                   FDBFILE.FDB_DDECIMAL_94,
                   ISNULL(FDBFILE.FDB_DNAME_95,''),
                   FDBFILE.FDB_DOFFSET_95,
                   FDBFILE.FDB_DDIGITS_95,
                   FDBFILE.FDB_DTYPE_95,
                   FDBFILE.FDB_DDECIMAL_95,
                   ISNULL(FDBFILE.FDB_DNAME_96,''),
                   FDBFILE.FDB_DOFFSET_96,
                   FDBFILE.FDB_DDIGITS_96,
                   FDBFILE.FDB_DTYPE_96,
                   FDBFILE.FDB_DDECIMAL_96,
                   ISNULL(FDBFILE.FDB_DNAME_97,''),
                   FDBFILE.FDB_DOFFSET_97,
                   FDBFILE.FDB_DDIGITS_97,
                   FDBFILE.FDB_DTYPE_97,
                   FDBFILE.FDB_DDECIMAL_97,
                   ISNULL(FDBFILE.FDB_DNAME_98,''),
                   FDBFILE.FDB_DOFFSET_98,
                   FDBFILE.FDB_DDIGITS_98,
                   FDBFILE.FDB_DTYPE_98,
                   FDBFILE.FDB_DDECIMAL_98,
                   ISNULL(FDBFILE.FDB_DNAME_99,''),
                   FDBFILE.FDB_DOFFSET_99,
                   FDBFILE.FDB_DDIGITS_99,
                   FDBFILE.FDB_DTYPE_99,
                   FDBFILE.FDB_DDECIMAL_99,
                   ISNULL(FDBFILE.FDB_DNAME_100,''),
                   FDBFILE.FDB_DOFFSET_100,
                   FDBFILE.FDB_DDIGITS_100,
                   FDBFILE.FDB_DTYPE_100,
                   FDBFILE.FDB_DDECIMAL_100,
                   ISNULL(FDBFILE.FDB_DNAME_101,''),
                   FDBFILE.FDB_DOFFSET_101,
                   FDBFILE.FDB_DDIGITS_101,
                   FDBFILE.FDB_DTYPE_101,
                   FDBFILE.FDB_DDECIMAL_101,
                   ISNULL(FDBFILE.FDB_DNAME_102,''),
                   FDBFILE.FDB_DOFFSET_102,
                   FDBFILE.FDB_DDIGITS_102,
                   FDBFILE.FDB_DTYPE_102,
                   FDBFILE.FDB_DDECIMAL_102,
                   ISNULL(FDBFILE.FDB_DNAME_103,''),
                   FDBFILE.FDB_DOFFSET_103,
                   FDBFILE.FDB_DDIGITS_103,
                   FDBFILE.FDB_DTYPE_103,
                   FDBFILE.FDB_DDECIMAL_103,
                   ISNULL(FDBFILE.FDB_DNAME_104,''),
                   FDBFILE.FDB_DOFFSET_104,
                   FDBFILE.FDB_DDIGITS_104,
                   FDBFILE.FDB_DTYPE_104,
                   FDBFILE.FDB_DDECIMAL_104,
                   ISNULL(FDBFILE.FDB_DNAME_105,''),
                   FDBFILE.FDB_DOFFSET_105,
                   FDBFILE.FDB_DDIGITS_105,
                   FDBFILE.FDB_DTYPE_105,
                   FDBFILE.FDB_DDECIMAL_105,
                   ISNULL(FDBFILE.FDB_DNAME_106,''),
                   FDBFILE.FDB_DOFFSET_106,
                   FDBFILE.FDB_DDIGITS_106,
                   FDBFILE.FDB_DTYPE_106,
                   FDBFILE.FDB_DDECIMAL_106,
                   ISNULL(FDBFILE.FDB_DNAME_107,''),
                   FDBFILE.FDB_DOFFSET_107,
                   FDBFILE.FDB_DDIGITS_107,
                   FDBFILE.FDB_DTYPE_107,
                   FDBFILE.FDB_DDECIMAL_107,
                   ISNULL(FDBFILE.FDB_DNAME_108,''),
                   FDBFILE.FDB_DOFFSET_108,
                   FDBFILE.FDB_DDIGITS_108,
                   FDBFILE.FDB_DTYPE_108,
                   FDBFILE.FDB_DDECIMAL_108,
                   ISNULL(FDBFILE.FDB_DNAME_109,''),
                   FDBFILE.FDB_DOFFSET_109,
                   FDBFILE.FDB_DDIGITS_109,
                   FDBFILE.FDB_DTYPE_109,
                   FDBFILE.FDB_DDECIMAL_109,
                   ISNULL(FDBFILE.FDB_DNAME_110,''),
                   FDBFILE.FDB_DOFFSET_110,
                   FDBFILE.FDB_DDIGITS_110,
                   FDBFILE.FDB_DTYPE_110,
                   FDBFILE.FDB_DDECIMAL_110,
                   ISNULL(FDBFILE.FDB_DNAME_111,''),
                   FDBFILE.FDB_DOFFSET_111,
                   FDBFILE.FDB_DDIGITS_111,
                   FDBFILE.FDB_DTYPE_111,
                   FDBFILE.FDB_DDECIMAL_111,
                   ISNULL(FDBFILE.FDB_DNAME_112,''),
                   FDBFILE.FDB_DOFFSET_112,
                   FDBFILE.FDB_DDIGITS_112,
                   FDBFILE.FDB_DTYPE_112,
                   FDBFILE.FDB_DDECIMAL_112,
                   ISNULL(FDBFILE.FDB_DNAME_113,''),
                   FDBFILE.FDB_DOFFSET_113,
                   FDBFILE.FDB_DDIGITS_113,
                   FDBFILE.FDB_DTYPE_113,
                   FDBFILE.FDB_DDECIMAL_113,
                   ISNULL(FDBFILE.FDB_DNAME_114,''),
                   FDBFILE.FDB_DOFFSET_114,
                   FDBFILE.FDB_DDIGITS_114,
                   FDBFILE.FDB_DTYPE_114,
                   FDBFILE.FDB_DDECIMAL_114,
                   ISNULL(FDBFILE.FDB_DNAME_115,''),
                   FDBFILE.FDB_DOFFSET_115,
                   FDBFILE.FDB_DDIGITS_115,
                   FDBFILE.FDB_DTYPE_115,
                   FDBFILE.FDB_DDECIMAL_115,
                   ISNULL(FDBFILE.FDB_DNAME_116,''),
                   FDBFILE.FDB_DOFFSET_116,
                   FDBFILE.FDB_DDIGITS_116,
                   FDBFILE.FDB_DTYPE_116,
                   FDBFILE.FDB_DDECIMAL_116,
                   ISNULL(FDBFILE.FDB_DNAME_117,''),
                   FDBFILE.FDB_DOFFSET_117,
                   FDBFILE.FDB_DDIGITS_117,
                   FDBFILE.FDB_DTYPE_117,
                   FDBFILE.FDB_DDECIMAL_117,
                   ISNULL(FDBFILE.FDB_DNAME_118,''),
                   FDBFILE.FDB_DOFFSET_118,
                   FDBFILE.FDB_DDIGITS_118,
                   FDBFILE.FDB_DTYPE_118,
                   FDBFILE.FDB_DDECIMAL_118,
                   ISNULL(FDBFILE.FDB_DNAME_119,''),
                   FDBFILE.FDB_DOFFSET_119,
                   FDBFILE.FDB_DDIGITS_119,
                   FDBFILE.FDB_DTYPE_119,
                   FDBFILE.FDB_DDECIMAL_119,
                   ISNULL(FDBFILE.FDB_DNAME_120,''),
                   FDBFILE.FDB_DOFFSET_120,
                   FDBFILE.FDB_DDIGITS_120,
                   FDBFILE.FDB_DTYPE_120,
                   FDBFILE.FDB_DDECIMAL_120,
                   ISNULL(FDBFILE.FDB_DNAME_121,''),
                   FDBFILE.FDB_DOFFSET_121,
                   FDBFILE.FDB_DDIGITS_121,
                   FDBFILE.FDB_DTYPE_121,
                   FDBFILE.FDB_DDECIMAL_121,
                   ISNULL(FDBFILE.FDB_DNAME_122,''),
                   FDBFILE.FDB_DOFFSET_122,
                   FDBFILE.FDB_DDIGITS_122,
                   FDBFILE.FDB_DTYPE_122,
                   FDBFILE.FDB_DDECIMAL_122,
                   ISNULL(FDBFILE.FDB_DNAME_123,''),
                   FDBFILE.FDB_DOFFSET_123,
                   FDBFILE.FDB_DDIGITS_123,
                   FDBFILE.FDB_DTYPE_123,
                   FDBFILE.FDB_DDECIMAL_123,
                   ISNULL(FDBFILE.FDB_DNAME_124,''),
                   FDBFILE.FDB_DOFFSET_124,
                   FDBFILE.FDB_DDIGITS_124,
                   FDBFILE.FDB_DTYPE_124,
                   FDBFILE.FDB_DDECIMAL_124,
                   ISNULL(FDBFILE.FDB_DNAME_125,''),
                   FDBFILE.FDB_DOFFSET_125,
                   FDBFILE.FDB_DDIGITS_125,
                   FDBFILE.FDB_DTYPE_125,
                   FDBFILE.FDB_DDECIMAL_125,
                   ISNULL(FDBFILE.FDB_DNAME_126,''),
                   FDBFILE.FDB_DOFFSET_126,
                   FDBFILE.FDB_DDIGITS_126,
                   FDBFILE.FDB_DTYPE_126,
                   FDBFILE.FDB_DDECIMAL_126,
                   ISNULL(FDBFILE.FDB_DNAME_127,''),
                   FDBFILE.FDB_DOFFSET_127,
                   FDBFILE.FDB_DDIGITS_127,
                   FDBFILE.FDB_DTYPE_127,
                   FDBFILE.FDB_DDECIMAL_127,
                   ISNULL(FDBFILE.FDB_DNAME_128,''),
                   FDBFILE.FDB_DOFFSET_128,
                   FDBFILE.FDB_DDIGITS_128,
                   FDBFILE.FDB_DTYPE_128,
                   FDBFILE.FDB_DDECIMAL_128,
                   ISNULL(FDBFILE.FDB_DNAME_129,''),
                   FDBFILE.FDB_DOFFSET_129,
                   FDBFILE.FDB_DDIGITS_129,
                   FDBFILE.FDB_DTYPE_129,
                   FDBFILE.FDB_DDECIMAL_129,
                   ISNULL(FDBFILE.FDB_DNAME_130,''),
                   FDBFILE.FDB_DOFFSET_130,
                   FDBFILE.FDB_DDIGITS_130,
                   FDBFILE.FDB_DTYPE_130,
                   FDBFILE.FDB_DDECIMAL_130,
                   ISNULL(FDBFILE.FDB_DNAME_131,''),
                   FDBFILE.FDB_DOFFSET_131,
                   FDBFILE.FDB_DDIGITS_131,
                   FDBFILE.FDB_DTYPE_131,
                   FDBFILE.FDB_DDECIMAL_131,
                   ISNULL(FDBFILE.FDB_DNAME_132,''),
                   FDBFILE.FDB_DOFFSET_132,
                   FDBFILE.FDB_DDIGITS_132,
                   FDBFILE.FDB_DTYPE_132,
                   FDBFILE.FDB_DDECIMAL_132,
                   ISNULL(FDBFILE.FDB_DNAME_133,''),
                   FDBFILE.FDB_DOFFSET_133,
                   FDBFILE.FDB_DDIGITS_133,
                   FDBFILE.FDB_DTYPE_133,
                   FDBFILE.FDB_DDECIMAL_133,
                   ISNULL(FDBFILE.FDB_DNAME_134,''),
                   FDBFILE.FDB_DOFFSET_134,
                   FDBFILE.FDB_DDIGITS_134,
                   FDBFILE.FDB_DTYPE_134,
                   FDBFILE.FDB_DDECIMAL_134,
                   ISNULL(FDBFILE.FDB_DNAME_135,''),
                   FDBFILE.FDB_DOFFSET_135,
                   FDBFILE.FDB_DDIGITS_135,
                   FDBFILE.FDB_DTYPE_135,
                   FDBFILE.FDB_DDECIMAL_135,
                   ISNULL(FDBFILE.FDB_DNAME_136,''),
                   FDBFILE.FDB_DOFFSET_136,
                   FDBFILE.FDB_DDIGITS_136,
                   FDBFILE.FDB_DTYPE_136,
                   FDBFILE.FDB_DDECIMAL_136,
                   ISNULL(FDBFILE.FDB_DNAME_137,''),
                   FDBFILE.FDB_DOFFSET_137,
                   FDBFILE.FDB_DDIGITS_137,
                   FDBFILE.FDB_DTYPE_137,
                   FDBFILE.FDB_DDECIMAL_137,
                   ISNULL(FDBFILE.FDB_DNAME_138,''),
                   FDBFILE.FDB_DOFFSET_138,
                   FDBFILE.FDB_DDIGITS_138,
                   FDBFILE.FDB_DTYPE_138,
                   FDBFILE.FDB_DDECIMAL_138,
                   ISNULL(FDBFILE.FDB_DNAME_139,''),
                   FDBFILE.FDB_DOFFSET_139,
                   FDBFILE.FDB_DDIGITS_139,
                   FDBFILE.FDB_DTYPE_139,
                   FDBFILE.FDB_DDECIMAL_139,
                   ISNULL(FDBFILE.FDB_DNAME_140,''),
                   FDBFILE.FDB_DOFFSET_140,
                   FDBFILE.FDB_DDIGITS_140,
                   FDBFILE.FDB_DTYPE_140,
                   FDBFILE.FDB_DDECIMAL_140,
                   ISNULL(FDBFILE.FDB_DNAME_141,''),
                   FDBFILE.FDB_DOFFSET_141,
                   FDBFILE.FDB_DDIGITS_141,
                   FDBFILE.FDB_DTYPE_141,
                   FDBFILE.FDB_DDECIMAL_141,
                   ISNULL(FDBFILE.FDB_DNAME_142,''),
                   FDBFILE.FDB_DOFFSET_142,
                   FDBFILE.FDB_DDIGITS_142,
                   FDBFILE.FDB_DTYPE_142,
                   FDBFILE.FDB_DDECIMAL_142,
                   ISNULL(FDBFILE.FDB_DNAME_143,''),
                   FDBFILE.FDB_DOFFSET_143,
                   FDBFILE.FDB_DDIGITS_143,
                   FDBFILE.FDB_DTYPE_143,
                   FDBFILE.FDB_DDECIMAL_143,
                   ISNULL(FDBFILE.FDB_DNAME_144,''),
                   FDBFILE.FDB_DOFFSET_144,
                   FDBFILE.FDB_DDIGITS_144,
                   FDBFILE.FDB_DTYPE_144,
                   FDBFILE.FDB_DDECIMAL_144,
                   ISNULL(FDBFILE.FDB_DNAME_145,''),
                   FDBFILE.FDB_DOFFSET_145,
                   FDBFILE.FDB_DDIGITS_145,
                   FDBFILE.FDB_DTYPE_145,
                   FDBFILE.FDB_DDECIMAL_145,
                   ISNULL(FDBFILE.FDB_DNAME_146,''),
                   FDBFILE.FDB_DOFFSET_146,
                   FDBFILE.FDB_DDIGITS_146,
                   FDBFILE.FDB_DTYPE_146,
                   FDBFILE.FDB_DDECIMAL_146,
                   ISNULL(FDBFILE.FDB_DNAME_147,''),
                   FDBFILE.FDB_DOFFSET_147,
                   FDBFILE.FDB_DDIGITS_147,
                   FDBFILE.FDB_DTYPE_147,
                   FDBFILE.FDB_DDECIMAL_147,
                   ISNULL(FDBFILE.FDB_DNAME_148,''),
                   FDBFILE.FDB_DOFFSET_148,
                   FDBFILE.FDB_DDIGITS_148,
                   FDBFILE.FDB_DTYPE_148,
                   FDBFILE.FDB_DDECIMAL_148,
                   ISNULL(FDBFILE.FDB_DNAME_149,''),
                   FDBFILE.FDB_DOFFSET_149,
                   FDBFILE.FDB_DDIGITS_149,
                   FDBFILE.FDB_DTYPE_149,
                   FDBFILE.FDB_DDECIMAL_149,
                   ISNULL(FDBFILE.FDB_DNAME_150,''),
                   FDBFILE.FDB_DOFFSET_150,
                   FDBFILE.FDB_DDIGITS_150,
                   FDBFILE.FDB_DTYPE_150,
                   FDBFILE.FDB_DDECIMAL_150,
                   ISNULL(FDBFILE.FDB_DNAME_151,''),
                   FDBFILE.FDB_DOFFSET_151,
                   FDBFILE.FDB_DDIGITS_151,
                   FDBFILE.FDB_DTYPE_151,
                   FDBFILE.FDB_DDECIMAL_151,
                   ISNULL(FDBFILE.FDB_DNAME_152,''),
                   FDBFILE.FDB_DOFFSET_152,
                   FDBFILE.FDB_DDIGITS_152,
                   FDBFILE.FDB_DTYPE_152,
                   FDBFILE.FDB_DDECIMAL_152,
                   ISNULL(FDBFILE.FDB_DNAME_153,''),
                   FDBFILE.FDB_DOFFSET_153,
                   FDBFILE.FDB_DDIGITS_153,
                   FDBFILE.FDB_DTYPE_153,
                   FDBFILE.FDB_DDECIMAL_153,
                   ISNULL(FDBFILE.FDB_DNAME_154,''),
                   FDBFILE.FDB_DOFFSET_154,
                   FDBFILE.FDB_DDIGITS_154,
                   FDBFILE.FDB_DTYPE_154,
                   FDBFILE.FDB_DDECIMAL_154,
                   ISNULL(FDBFILE.FDB_DNAME_155,''),
                   FDBFILE.FDB_DOFFSET_155,
                   FDBFILE.FDB_DDIGITS_155,
                   FDBFILE.FDB_DTYPE_155,
                   FDBFILE.FDB_DDECIMAL_155,
                   ISNULL(FDBFILE.FDB_DNAME_156,''),
                   FDBFILE.FDB_DOFFSET_156,
                   FDBFILE.FDB_DDIGITS_156,
                   FDBFILE.FDB_DTYPE_156,
                   FDBFILE.FDB_DDECIMAL_156,
                   ISNULL(FDBFILE.FDB_DNAME_157,''),
                   FDBFILE.FDB_DOFFSET_157,
                   FDBFILE.FDB_DDIGITS_157,
                   FDBFILE.FDB_DTYPE_157,
                   FDBFILE.FDB_DDECIMAL_157,
                   ISNULL(FDBFILE.FDB_DNAME_158,''),
                   FDBFILE.FDB_DOFFSET_158,
                   FDBFILE.FDB_DDIGITS_158,
                   FDBFILE.FDB_DTYPE_158,
                   FDBFILE.FDB_DDECIMAL_158,
                   ISNULL(FDBFILE.FDB_DNAME_159,''),
                   FDBFILE.FDB_DOFFSET_159,
                   FDBFILE.FDB_DDIGITS_159,
                   FDBFILE.FDB_DTYPE_159,
                   FDBFILE.FDB_DDECIMAL_159,
                   ISNULL(FDBFILE.FDB_DNAME_160,''),
                   FDBFILE.FDB_DOFFSET_160,
                   FDBFILE.FDB_DDIGITS_160,
                   FDBFILE.FDB_DTYPE_160,
                   FDBFILE.FDB_DDECIMAL_160,
                   ISNULL(FDBFILE.FDB_DNAME_161,''),
                   FDBFILE.FDB_DOFFSET_161,
                   FDBFILE.FDB_DDIGITS_161,
                   FDBFILE.FDB_DTYPE_161,
                   FDBFILE.FDB_DDECIMAL_161,
                   ISNULL(FDBFILE.FDB_DNAME_162,''),
                   FDBFILE.FDB_DOFFSET_162,
                   FDBFILE.FDB_DDIGITS_162,
                   FDBFILE.FDB_DTYPE_162,
                   FDBFILE.FDB_DDECIMAL_162,
                   ISNULL(FDBFILE.FDB_DNAME_163,''),
                   FDBFILE.FDB_DOFFSET_163,
                   FDBFILE.FDB_DDIGITS_163,
                   FDBFILE.FDB_DTYPE_163,
                   FDBFILE.FDB_DDECIMAL_163,
                   ISNULL(FDBFILE.FDB_DNAME_164,''),
                   FDBFILE.FDB_DOFFSET_164,
                   FDBFILE.FDB_DDIGITS_164,
                   FDBFILE.FDB_DTYPE_164,
                   FDBFILE.FDB_DDECIMAL_164,
                   ISNULL(FDBFILE.FDB_DNAME_165,''),
                   FDBFILE.FDB_DOFFSET_165,
                   FDBFILE.FDB_DDIGITS_165,
                   FDBFILE.FDB_DTYPE_165,
                   FDBFILE.FDB_DDECIMAL_165,
                   ISNULL(FDBFILE.FDB_DNAME_166,''),
                   FDBFILE.FDB_DOFFSET_166,
                   FDBFILE.FDB_DDIGITS_166,
                   FDBFILE.FDB_DTYPE_166,
                   FDBFILE.FDB_DDECIMAL_166,
                   ISNULL(FDBFILE.FDB_DNAME_167,''),
                   FDBFILE.FDB_DOFFSET_167,
                   FDBFILE.FDB_DDIGITS_167,
                   FDBFILE.FDB_DTYPE_167,
                   FDBFILE.FDB_DDECIMAL_167,
                   ISNULL(FDBFILE.FDB_DNAME_168,''),
                   FDBFILE.FDB_DOFFSET_168,
                   FDBFILE.FDB_DDIGITS_168,
                   FDBFILE.FDB_DTYPE_168,
                   FDBFILE.FDB_DDECIMAL_168,
                   ISNULL(FDBFILE.FDB_DNAME_169,''),
                   FDBFILE.FDB_DOFFSET_169,
                   FDBFILE.FDB_DDIGITS_169,
                   FDBFILE.FDB_DTYPE_169,
                   FDBFILE.FDB_DDECIMAL_169,
                   ISNULL(FDBFILE.FDB_DNAME_170,''),
                   FDBFILE.FDB_DOFFSET_170,
                   FDBFILE.FDB_DDIGITS_170,
                   FDBFILE.FDB_DTYPE_170,
                   FDBFILE.FDB_DDECIMAL_170,
                   ISNULL(FDBFILE.FDB_DNAME_171,''),
                   FDBFILE.FDB_DOFFSET_171,
                   FDBFILE.FDB_DDIGITS_171,
                   FDBFILE.FDB_DTYPE_171,
                   FDBFILE.FDB_DDECIMAL_171,
                   ISNULL(FDBFILE.FDB_DNAME_172,''),
                   FDBFILE.FDB_DOFFSET_172,
                   FDBFILE.FDB_DDIGITS_172,
                   FDBFILE.FDB_DTYPE_172,
                   FDBFILE.FDB_DDECIMAL_172,
                   ISNULL(FDBFILE.FDB_DNAME_173,''),
                   FDBFILE.FDB_DOFFSET_173,
                   FDBFILE.FDB_DDIGITS_173,
                   FDBFILE.FDB_DTYPE_173,
                   FDBFILE.FDB_DDECIMAL_173,
                   ISNULL(FDBFILE.FDB_DNAME_174,''),
                   FDBFILE.FDB_DOFFSET_174,
                   FDBFILE.FDB_DDIGITS_174,
                   FDBFILE.FDB_DTYPE_174,
                   FDBFILE.FDB_DDECIMAL_174,
                   ISNULL(FDBFILE.FDB_DNAME_175,''),
                   FDBFILE.FDB_DOFFSET_175,
                   FDBFILE.FDB_DDIGITS_175,
                   FDBFILE.FDB_DTYPE_175,
                   FDBFILE.FDB_DDECIMAL_175,
                   ISNULL(FDBFILE.FDB_DNAME_176,''),
                   FDBFILE.FDB_DOFFSET_176,
                   FDBFILE.FDB_DDIGITS_176,
                   FDBFILE.FDB_DTYPE_176,
                   FDBFILE.FDB_DDECIMAL_176,
                   ISNULL(FDBFILE.FDB_DNAME_177,''),
                   FDBFILE.FDB_DOFFSET_177,
                   FDBFILE.FDB_DDIGITS_177,
                   FDBFILE.FDB_DTYPE_177,
                   FDBFILE.FDB_DDECIMAL_177,
                   ISNULL(FDBFILE.FDB_DNAME_178,''),
                   FDBFILE.FDB_DOFFSET_178,
                   FDBFILE.FDB_DDIGITS_178,
                   FDBFILE.FDB_DTYPE_178,
                   FDBFILE.FDB_DDECIMAL_178,
                   ISNULL(FDBFILE.FDB_DNAME_179,''),
                   FDBFILE.FDB_DOFFSET_179,
                   FDBFILE.FDB_DDIGITS_179,
                   FDBFILE.FDB_DTYPE_179,
                   FDBFILE.FDB_DDECIMAL_179,
                   ISNULL(FDBFILE.FDB_DNAME_180,''),
                   FDBFILE.FDB_DOFFSET_180,
                   FDBFILE.FDB_DDIGITS_180,
                   FDBFILE.FDB_DTYPE_180,
                   FDBFILE.FDB_DDECIMAL_180,
                   ISNULL(FDBFILE.FDB_DNAME_181,''),
                   FDBFILE.FDB_DOFFSET_181,
                   FDBFILE.FDB_DDIGITS_181,
                   FDBFILE.FDB_DTYPE_181,
                   FDBFILE.FDB_DDECIMAL_181,
                   ISNULL(FDBFILE.FDB_DNAME_182,''),
                   FDBFILE.FDB_DOFFSET_182,
                   FDBFILE.FDB_DDIGITS_182,
                   FDBFILE.FDB_DTYPE_182,
                   FDBFILE.FDB_DDECIMAL_182,
                   ISNULL(FDBFILE.FDB_DNAME_183,''),
                   FDBFILE.FDB_DOFFSET_183,
                   FDBFILE.FDB_DDIGITS_183,
                   FDBFILE.FDB_DTYPE_183,
                   FDBFILE.FDB_DDECIMAL_183,
                   ISNULL(FDBFILE.FDB_DNAME_184,''),
                   FDBFILE.FDB_DOFFSET_184,
                   FDBFILE.FDB_DDIGITS_184,
                   FDBFILE.FDB_DTYPE_184,
                   FDBFILE.FDB_DDECIMAL_184,
                   ISNULL(FDBFILE.FDB_DNAME_185,''),
                   FDBFILE.FDB_DOFFSET_185,
                   FDBFILE.FDB_DDIGITS_185,
                   FDBFILE.FDB_DTYPE_185,
                   FDBFILE.FDB_DDECIMAL_185,
                   ISNULL(FDBFILE.FDB_DNAME_186,''),
                   FDBFILE.FDB_DOFFSET_186,
                   FDBFILE.FDB_DDIGITS_186,
                   FDBFILE.FDB_DTYPE_186,
                   FDBFILE.FDB_DDECIMAL_186,
                   ISNULL(FDBFILE.FDB_DNAME_187,''),
                   FDBFILE.FDB_DOFFSET_187,
                   FDBFILE.FDB_DDIGITS_187,
                   FDBFILE.FDB_DTYPE_187,
                   FDBFILE.FDB_DDECIMAL_187,
                   ISNULL(FDBFILE.FDB_DNAME_188,''),
                   FDBFILE.FDB_DOFFSET_188,
                   FDBFILE.FDB_DDIGITS_188,
                   FDBFILE.FDB_DTYPE_188,
                   FDBFILE.FDB_DDECIMAL_188,
                   ISNULL(FDBFILE.FDB_DNAME_189,''),
                   FDBFILE.FDB_DOFFSET_189,
                   FDBFILE.FDB_DDIGITS_189,
                   FDBFILE.FDB_DTYPE_189,
                   FDBFILE.FDB_DDECIMAL_189,
                   ISNULL(FDBFILE.FDB_DNAME_190,''),
                   FDBFILE.FDB_DOFFSET_190,
                   FDBFILE.FDB_DDIGITS_190,
                   FDBFILE.FDB_DTYPE_190,
                   FDBFILE.FDB_DDECIMAL_190,
                   ISNULL(FDBFILE.FDB_DNAME_191,''),
                   FDBFILE.FDB_DOFFSET_191,
                   FDBFILE.FDB_DDIGITS_191,
                   FDBFILE.FDB_DTYPE_191,
                   FDBFILE.FDB_DDECIMAL_191,
                   ISNULL(FDBFILE.FDB_DNAME_192,''),
                   FDBFILE.FDB_DOFFSET_192,
                   FDBFILE.FDB_DDIGITS_192,
                   FDBFILE.FDB_DTYPE_192,
                   FDBFILE.FDB_DDECIMAL_192,
                   ISNULL(FDBFILE.FDB_DNAME_193,''),
                   FDBFILE.FDB_DOFFSET_193,
                   FDBFILE.FDB_DDIGITS_193,
                   FDBFILE.FDB_DTYPE_193,
                   FDBFILE.FDB_DDECIMAL_193,
                   ISNULL(FDBFILE.FDB_DNAME_194,''),
                   FDBFILE.FDB_DOFFSET_194,
                   FDBFILE.FDB_DDIGITS_194,
                   FDBFILE.FDB_DTYPE_194,
                   FDBFILE.FDB_DDECIMAL_194,
                   ISNULL(FDBFILE.FDB_DNAME_195,''),
                   FDBFILE.FDB_DOFFSET_195,
                   FDBFILE.FDB_DDIGITS_195,
                   FDBFILE.FDB_DTYPE_195,
                   FDBFILE.FDB_DDECIMAL_195,
                   ISNULL(FDBFILE.FDB_DNAME_196,''),
                   FDBFILE.FDB_DOFFSET_196,
                   FDBFILE.FDB_DDIGITS_196,
                   FDBFILE.FDB_DTYPE_196,
                   FDBFILE.FDB_DDECIMAL_196,
                   ISNULL(FDBFILE.FDB_DNAME_197,''),
                   FDBFILE.FDB_DOFFSET_197,
                   FDBFILE.FDB_DDIGITS_197,
                   FDBFILE.FDB_DTYPE_197,
                   FDBFILE.FDB_DDECIMAL_197,
                   ISNULL(FDBFILE.FDB_DNAME_198,''),
                   FDBFILE.FDB_DOFFSET_198,
                   FDBFILE.FDB_DDIGITS_198,
                   FDBFILE.FDB_DTYPE_198,
                   FDBFILE.FDB_DDECIMAL_198,
                   ISNULL(FDBFILE.FDB_DNAME_199,''),
                   FDBFILE.FDB_DOFFSET_199,
                   FDBFILE.FDB_DDIGITS_199,
                   FDBFILE.FDB_DTYPE_199,
                   FDBFILE.FDB_DDECIMAL_199,
                   ISNULL(FDBFILE.FDB_DNAME_200,''),
                   FDBFILE.FDB_DOFFSET_200,
                   FDBFILE.FDB_DDIGITS_200,
                   FDBFILE.FDB_DTYPE_200,
                   FDBFILE.FDB_DDECIMAL_200,
                   ISNULL(FDBFILE.FDB_CALLID_1,''),
                   FDBFILE.FDB_KEYLOC_1,
                   ISNULL(FDBFILE.FDB_CALLID_2,''),
                   FDBFILE.FDB_KEYLOC_2
            FROM DBO.FDBFILE
            WHERE FDBFILE.FDB_DBNO >= :QFDB1-WBEG-DBNO
              AND FDBFILE.FDB_DBNO <= :QFDB1-WEND-DBNO
            ORDER BY FDBFILE.FDB_DBNO
           END-EXEC.

           EXEC SQL
               OPEN FDB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFDB1-WBEG-DBNO
                          QFDB1-WEND-DBNO.

           MOVE STAT---GOOD TO FDB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-FDB1-FILE.
           PERFORM READ-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.

           MOVE FDB-DBNO   TO QFDB-DBNO.

           EXEC SQL
            SELECT FDBFILE.FDB_DBNO,
                   ISNULL(CAST(FDBFILE.FDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FDBFILE.FDB_DNAME_1,''),
                   FDBFILE.FDB_DOFFSET_1,
                   FDBFILE.FDB_DDIGITS_1,
                   FDBFILE.FDB_DTYPE_1,
                   FDBFILE.FDB_DDECIMAL_1,
                   ISNULL(FDBFILE.FDB_DNAME_2,''),
                   FDBFILE.FDB_DOFFSET_2,
                   FDBFILE.FDB_DDIGITS_2,
                   FDBFILE.FDB_DTYPE_2,
                   FDBFILE.FDB_DDECIMAL_2,
                   ISNULL(FDBFILE.FDB_DNAME_3,''),
                   FDBFILE.FDB_DOFFSET_3,
                   FDBFILE.FDB_DDIGITS_3,
                   FDBFILE.FDB_DTYPE_3,
                   FDBFILE.FDB_DDECIMAL_3,
                   ISNULL(FDBFILE.FDB_DNAME_4,''),
                   FDBFILE.FDB_DOFFSET_4,
                   FDBFILE.FDB_DDIGITS_4,
                   FDBFILE.FDB_DTYPE_4,
                   FDBFILE.FDB_DDECIMAL_4,
                   ISNULL(FDBFILE.FDB_DNAME_5,''),
                   FDBFILE.FDB_DOFFSET_5,
                   FDBFILE.FDB_DDIGITS_5,
                   FDBFILE.FDB_DTYPE_5,
                   FDBFILE.FDB_DDECIMAL_5,
                   ISNULL(FDBFILE.FDB_DNAME_6,''),
                   FDBFILE.FDB_DOFFSET_6,
                   FDBFILE.FDB_DDIGITS_6,
                   FDBFILE.FDB_DTYPE_6,
                   FDBFILE.FDB_DDECIMAL_6,
                   ISNULL(FDBFILE.FDB_DNAME_7,''),
                   FDBFILE.FDB_DOFFSET_7,
                   FDBFILE.FDB_DDIGITS_7,
                   FDBFILE.FDB_DTYPE_7,
                   FDBFILE.FDB_DDECIMAL_7,
                   ISNULL(FDBFILE.FDB_DNAME_8,''),
                   FDBFILE.FDB_DOFFSET_8,
                   FDBFILE.FDB_DDIGITS_8,
                   FDBFILE.FDB_DTYPE_8,
                   FDBFILE.FDB_DDECIMAL_8,
                   ISNULL(FDBFILE.FDB_DNAME_9,''),
                   FDBFILE.FDB_DOFFSET_9,
                   FDBFILE.FDB_DDIGITS_9,
                   FDBFILE.FDB_DTYPE_9,
                   FDBFILE.FDB_DDECIMAL_9,
                   ISNULL(FDBFILE.FDB_DNAME_10,''),
                   FDBFILE.FDB_DOFFSET_10,
                   FDBFILE.FDB_DDIGITS_10,
                   FDBFILE.FDB_DTYPE_10,
                   FDBFILE.FDB_DDECIMAL_10,
                   ISNULL(FDBFILE.FDB_DNAME_11,''),
                   FDBFILE.FDB_DOFFSET_11,
                   FDBFILE.FDB_DDIGITS_11,
                   FDBFILE.FDB_DTYPE_11,
                   FDBFILE.FDB_DDECIMAL_11,
                   ISNULL(FDBFILE.FDB_DNAME_12,''),
                   FDBFILE.FDB_DOFFSET_12,
                   FDBFILE.FDB_DDIGITS_12,
                   FDBFILE.FDB_DTYPE_12,
                   FDBFILE.FDB_DDECIMAL_12,
                   ISNULL(FDBFILE.FDB_DNAME_13,''),
                   FDBFILE.FDB_DOFFSET_13,
                   FDBFILE.FDB_DDIGITS_13,
                   FDBFILE.FDB_DTYPE_13,
                   FDBFILE.FDB_DDECIMAL_13,
                   ISNULL(FDBFILE.FDB_DNAME_14,''),
                   FDBFILE.FDB_DOFFSET_14,
                   FDBFILE.FDB_DDIGITS_14,
                   FDBFILE.FDB_DTYPE_14,
                   FDBFILE.FDB_DDECIMAL_14,
                   ISNULL(FDBFILE.FDB_DNAME_15,''),
                   FDBFILE.FDB_DOFFSET_15,
                   FDBFILE.FDB_DDIGITS_15,
                   FDBFILE.FDB_DTYPE_15,
                   FDBFILE.FDB_DDECIMAL_15,
                   ISNULL(FDBFILE.FDB_DNAME_16,''),
                   FDBFILE.FDB_DOFFSET_16,
                   FDBFILE.FDB_DDIGITS_16,
                   FDBFILE.FDB_DTYPE_16,
                   FDBFILE.FDB_DDECIMAL_16,
                   ISNULL(FDBFILE.FDB_DNAME_17,''),
                   FDBFILE.FDB_DOFFSET_17,
                   FDBFILE.FDB_DDIGITS_17,
                   FDBFILE.FDB_DTYPE_17,
                   FDBFILE.FDB_DDECIMAL_17,
                   ISNULL(FDBFILE.FDB_DNAME_18,''),
                   FDBFILE.FDB_DOFFSET_18,
                   FDBFILE.FDB_DDIGITS_18,
                   FDBFILE.FDB_DTYPE_18,
                   FDBFILE.FDB_DDECIMAL_18,
                   ISNULL(FDBFILE.FDB_DNAME_19,''),
                   FDBFILE.FDB_DOFFSET_19,
                   FDBFILE.FDB_DDIGITS_19,
                   FDBFILE.FDB_DTYPE_19,
                   FDBFILE.FDB_DDECIMAL_19,
                   ISNULL(FDBFILE.FDB_DNAME_20,''),
                   FDBFILE.FDB_DOFFSET_20,
                   FDBFILE.FDB_DDIGITS_20,
                   FDBFILE.FDB_DTYPE_20,
                   FDBFILE.FDB_DDECIMAL_20,
                   ISNULL(FDBFILE.FDB_DNAME_21,''),
                   FDBFILE.FDB_DOFFSET_21,
                   FDBFILE.FDB_DDIGITS_21,
                   FDBFILE.FDB_DTYPE_21,
                   FDBFILE.FDB_DDECIMAL_21,
                   ISNULL(FDBFILE.FDB_DNAME_22,''),
                   FDBFILE.FDB_DOFFSET_22,
                   FDBFILE.FDB_DDIGITS_22,
                   FDBFILE.FDB_DTYPE_22,
                   FDBFILE.FDB_DDECIMAL_22,
                   ISNULL(FDBFILE.FDB_DNAME_23,''),
                   FDBFILE.FDB_DOFFSET_23,
                   FDBFILE.FDB_DDIGITS_23,
                   FDBFILE.FDB_DTYPE_23,
                   FDBFILE.FDB_DDECIMAL_23,
                   ISNULL(FDBFILE.FDB_DNAME_24,''),
                   FDBFILE.FDB_DOFFSET_24,
                   FDBFILE.FDB_DDIGITS_24,
                   FDBFILE.FDB_DTYPE_24,
                   FDBFILE.FDB_DDECIMAL_24,
                   ISNULL(FDBFILE.FDB_DNAME_25,''),
                   FDBFILE.FDB_DOFFSET_25,
                   FDBFILE.FDB_DDIGITS_25,
                   FDBFILE.FDB_DTYPE_25,
                   FDBFILE.FDB_DDECIMAL_25,
                   ISNULL(FDBFILE.FDB_DNAME_26,''),
                   FDBFILE.FDB_DOFFSET_26,
                   FDBFILE.FDB_DDIGITS_26,
                   FDBFILE.FDB_DTYPE_26,
                   FDBFILE.FDB_DDECIMAL_26,
                   ISNULL(FDBFILE.FDB_DNAME_27,''),
                   FDBFILE.FDB_DOFFSET_27,
                   FDBFILE.FDB_DDIGITS_27,
                   FDBFILE.FDB_DTYPE_27,
                   FDBFILE.FDB_DDECIMAL_27,
                   ISNULL(FDBFILE.FDB_DNAME_28,''),
                   FDBFILE.FDB_DOFFSET_28,
                   FDBFILE.FDB_DDIGITS_28,
                   FDBFILE.FDB_DTYPE_28,
                   FDBFILE.FDB_DDECIMAL_28,
                   ISNULL(FDBFILE.FDB_DNAME_29,''),
                   FDBFILE.FDB_DOFFSET_29,
                   FDBFILE.FDB_DDIGITS_29,
                   FDBFILE.FDB_DTYPE_29,
                   FDBFILE.FDB_DDECIMAL_29,
                   ISNULL(FDBFILE.FDB_DNAME_30,''),
                   FDBFILE.FDB_DOFFSET_30,
                   FDBFILE.FDB_DDIGITS_30,
                   FDBFILE.FDB_DTYPE_30,
                   FDBFILE.FDB_DDECIMAL_30,
                   ISNULL(FDBFILE.FDB_DNAME_31,''),
                   FDBFILE.FDB_DOFFSET_31,
                   FDBFILE.FDB_DDIGITS_31,
                   FDBFILE.FDB_DTYPE_31,
                   FDBFILE.FDB_DDECIMAL_31,
                   ISNULL(FDBFILE.FDB_DNAME_32,''),
                   FDBFILE.FDB_DOFFSET_32,
                   FDBFILE.FDB_DDIGITS_32,
                   FDBFILE.FDB_DTYPE_32,
                   FDBFILE.FDB_DDECIMAL_32,
                   ISNULL(FDBFILE.FDB_DNAME_33,''),
                   FDBFILE.FDB_DOFFSET_33,
                   FDBFILE.FDB_DDIGITS_33,
                   FDBFILE.FDB_DTYPE_33,
                   FDBFILE.FDB_DDECIMAL_33,
                   ISNULL(FDBFILE.FDB_DNAME_34,''),
                   FDBFILE.FDB_DOFFSET_34,
                   FDBFILE.FDB_DDIGITS_34,
                   FDBFILE.FDB_DTYPE_34,
                   FDBFILE.FDB_DDECIMAL_34,
                   ISNULL(FDBFILE.FDB_DNAME_35,''),
                   FDBFILE.FDB_DOFFSET_35,
                   FDBFILE.FDB_DDIGITS_35,
                   FDBFILE.FDB_DTYPE_35,
                   FDBFILE.FDB_DDECIMAL_35,
                   ISNULL(FDBFILE.FDB_DNAME_36,''),
                   FDBFILE.FDB_DOFFSET_36,
                   FDBFILE.FDB_DDIGITS_36,
                   FDBFILE.FDB_DTYPE_36,
                   FDBFILE.FDB_DDECIMAL_36,
                   ISNULL(FDBFILE.FDB_DNAME_37,''),
                   FDBFILE.FDB_DOFFSET_37,
                   FDBFILE.FDB_DDIGITS_37,
                   FDBFILE.FDB_DTYPE_37,
                   FDBFILE.FDB_DDECIMAL_37,
                   ISNULL(FDBFILE.FDB_DNAME_38,''),
                   FDBFILE.FDB_DOFFSET_38,
                   FDBFILE.FDB_DDIGITS_38,
                   FDBFILE.FDB_DTYPE_38,
                   FDBFILE.FDB_DDECIMAL_38,
                   ISNULL(FDBFILE.FDB_DNAME_39,''),
                   FDBFILE.FDB_DOFFSET_39,
                   FDBFILE.FDB_DDIGITS_39,
                   FDBFILE.FDB_DTYPE_39,
                   FDBFILE.FDB_DDECIMAL_39,
                   ISNULL(FDBFILE.FDB_DNAME_40,''),
                   FDBFILE.FDB_DOFFSET_40,
                   FDBFILE.FDB_DDIGITS_40,
                   FDBFILE.FDB_DTYPE_40,
                   FDBFILE.FDB_DDECIMAL_40,
                   ISNULL(FDBFILE.FDB_DNAME_41,''),
                   FDBFILE.FDB_DOFFSET_41,
                   FDBFILE.FDB_DDIGITS_41,
                   FDBFILE.FDB_DTYPE_41,
                   FDBFILE.FDB_DDECIMAL_41,
                   ISNULL(FDBFILE.FDB_DNAME_42,''),
                   FDBFILE.FDB_DOFFSET_42,
                   FDBFILE.FDB_DDIGITS_42,
                   FDBFILE.FDB_DTYPE_42,
                   FDBFILE.FDB_DDECIMAL_42,
                   ISNULL(FDBFILE.FDB_DNAME_43,''),
                   FDBFILE.FDB_DOFFSET_43,
                   FDBFILE.FDB_DDIGITS_43,
                   FDBFILE.FDB_DTYPE_43,
                   FDBFILE.FDB_DDECIMAL_43,
                   ISNULL(FDBFILE.FDB_DNAME_44,''),
                   FDBFILE.FDB_DOFFSET_44,
                   FDBFILE.FDB_DDIGITS_44,
                   FDBFILE.FDB_DTYPE_44,
                   FDBFILE.FDB_DDECIMAL_44,
                   ISNULL(FDBFILE.FDB_DNAME_45,''),
                   FDBFILE.FDB_DOFFSET_45,
                   FDBFILE.FDB_DDIGITS_45,
                   FDBFILE.FDB_DTYPE_45,
                   FDBFILE.FDB_DDECIMAL_45,
                   ISNULL(FDBFILE.FDB_DNAME_46,''),
                   FDBFILE.FDB_DOFFSET_46,
                   FDBFILE.FDB_DDIGITS_46,
                   FDBFILE.FDB_DTYPE_46,
                   FDBFILE.FDB_DDECIMAL_46,
                   ISNULL(FDBFILE.FDB_DNAME_47,''),
                   FDBFILE.FDB_DOFFSET_47,
                   FDBFILE.FDB_DDIGITS_47,
                   FDBFILE.FDB_DTYPE_47,
                   FDBFILE.FDB_DDECIMAL_47,
                   ISNULL(FDBFILE.FDB_DNAME_48,''),
                   FDBFILE.FDB_DOFFSET_48,
                   FDBFILE.FDB_DDIGITS_48,
                   FDBFILE.FDB_DTYPE_48,
                   FDBFILE.FDB_DDECIMAL_48,
                   ISNULL(FDBFILE.FDB_DNAME_49,''),
                   FDBFILE.FDB_DOFFSET_49,
                   FDBFILE.FDB_DDIGITS_49,
                   FDBFILE.FDB_DTYPE_49,
                   FDBFILE.FDB_DDECIMAL_49,
                   ISNULL(FDBFILE.FDB_DNAME_50,''),
                   FDBFILE.FDB_DOFFSET_50,
                   FDBFILE.FDB_DDIGITS_50,
                   FDBFILE.FDB_DTYPE_50,
                   FDBFILE.FDB_DDECIMAL_50,
                   ISNULL(FDBFILE.FDB_DNAME_51,''),
                   FDBFILE.FDB_DOFFSET_51,
                   FDBFILE.FDB_DDIGITS_51,
                   FDBFILE.FDB_DTYPE_51,
                   FDBFILE.FDB_DDECIMAL_51,
                   ISNULL(FDBFILE.FDB_DNAME_52,''),
                   FDBFILE.FDB_DOFFSET_52,
                   FDBFILE.FDB_DDIGITS_52,
                   FDBFILE.FDB_DTYPE_52,
                   FDBFILE.FDB_DDECIMAL_52,
                   ISNULL(FDBFILE.FDB_DNAME_53,''),
                   FDBFILE.FDB_DOFFSET_53,
                   FDBFILE.FDB_DDIGITS_53,
                   FDBFILE.FDB_DTYPE_53,
                   FDBFILE.FDB_DDECIMAL_53,
                   ISNULL(FDBFILE.FDB_DNAME_54,''),
                   FDBFILE.FDB_DOFFSET_54,
                   FDBFILE.FDB_DDIGITS_54,
                   FDBFILE.FDB_DTYPE_54,
                   FDBFILE.FDB_DDECIMAL_54,
                   ISNULL(FDBFILE.FDB_DNAME_55,''),
                   FDBFILE.FDB_DOFFSET_55,
                   FDBFILE.FDB_DDIGITS_55,
                   FDBFILE.FDB_DTYPE_55,
                   FDBFILE.FDB_DDECIMAL_55,
                   ISNULL(FDBFILE.FDB_DNAME_56,''),
                   FDBFILE.FDB_DOFFSET_56,
                   FDBFILE.FDB_DDIGITS_56,
                   FDBFILE.FDB_DTYPE_56,
                   FDBFILE.FDB_DDECIMAL_56,
                   ISNULL(FDBFILE.FDB_DNAME_57,''),
                   FDBFILE.FDB_DOFFSET_57,
                   FDBFILE.FDB_DDIGITS_57,
                   FDBFILE.FDB_DTYPE_57,
                   FDBFILE.FDB_DDECIMAL_57,
                   ISNULL(FDBFILE.FDB_DNAME_58,''),
                   FDBFILE.FDB_DOFFSET_58,
                   FDBFILE.FDB_DDIGITS_58,
                   FDBFILE.FDB_DTYPE_58,
                   FDBFILE.FDB_DDECIMAL_58,
                   ISNULL(FDBFILE.FDB_DNAME_59,''),
                   FDBFILE.FDB_DOFFSET_59,
                   FDBFILE.FDB_DDIGITS_59,
                   FDBFILE.FDB_DTYPE_59,
                   FDBFILE.FDB_DDECIMAL_59,
                   ISNULL(FDBFILE.FDB_DNAME_60,''),
                   FDBFILE.FDB_DOFFSET_60,
                   FDBFILE.FDB_DDIGITS_60,
                   FDBFILE.FDB_DTYPE_60,
                   FDBFILE.FDB_DDECIMAL_60,
                   ISNULL(FDBFILE.FDB_DNAME_61,''),
                   FDBFILE.FDB_DOFFSET_61,
                   FDBFILE.FDB_DDIGITS_61,
                   FDBFILE.FDB_DTYPE_61,
                   FDBFILE.FDB_DDECIMAL_61,
                   ISNULL(FDBFILE.FDB_DNAME_62,''),
                   FDBFILE.FDB_DOFFSET_62,
                   FDBFILE.FDB_DDIGITS_62,
                   FDBFILE.FDB_DTYPE_62,
                   FDBFILE.FDB_DDECIMAL_62,
                   ISNULL(FDBFILE.FDB_DNAME_63,''),
                   FDBFILE.FDB_DOFFSET_63,
                   FDBFILE.FDB_DDIGITS_63,
                   FDBFILE.FDB_DTYPE_63,
                   FDBFILE.FDB_DDECIMAL_63,
                   ISNULL(FDBFILE.FDB_DNAME_64,''),
                   FDBFILE.FDB_DOFFSET_64,
                   FDBFILE.FDB_DDIGITS_64,
                   FDBFILE.FDB_DTYPE_64,
                   FDBFILE.FDB_DDECIMAL_64,
                   ISNULL(FDBFILE.FDB_DNAME_65,''),
                   FDBFILE.FDB_DOFFSET_65,
                   FDBFILE.FDB_DDIGITS_65,
                   FDBFILE.FDB_DTYPE_65,
                   FDBFILE.FDB_DDECIMAL_65,
                   ISNULL(FDBFILE.FDB_DNAME_66,''),
                   FDBFILE.FDB_DOFFSET_66,
                   FDBFILE.FDB_DDIGITS_66,
                   FDBFILE.FDB_DTYPE_66,
                   FDBFILE.FDB_DDECIMAL_66,
                   ISNULL(FDBFILE.FDB_DNAME_67,''),
                   FDBFILE.FDB_DOFFSET_67,
                   FDBFILE.FDB_DDIGITS_67,
                   FDBFILE.FDB_DTYPE_67,
                   FDBFILE.FDB_DDECIMAL_67,
                   ISNULL(FDBFILE.FDB_DNAME_68,''),
                   FDBFILE.FDB_DOFFSET_68,
                   FDBFILE.FDB_DDIGITS_68,
                   FDBFILE.FDB_DTYPE_68,
                   FDBFILE.FDB_DDECIMAL_68,
                   ISNULL(FDBFILE.FDB_DNAME_69,''),
                   FDBFILE.FDB_DOFFSET_69,
                   FDBFILE.FDB_DDIGITS_69,
                   FDBFILE.FDB_DTYPE_69,
                   FDBFILE.FDB_DDECIMAL_69,
                   ISNULL(FDBFILE.FDB_DNAME_70,''),
                   FDBFILE.FDB_DOFFSET_70,
                   FDBFILE.FDB_DDIGITS_70,
                   FDBFILE.FDB_DTYPE_70,
                   FDBFILE.FDB_DDECIMAL_70,
                   ISNULL(FDBFILE.FDB_DNAME_71,''),
                   FDBFILE.FDB_DOFFSET_71,
                   FDBFILE.FDB_DDIGITS_71,
                   FDBFILE.FDB_DTYPE_71,
                   FDBFILE.FDB_DDECIMAL_71,
                   ISNULL(FDBFILE.FDB_DNAME_72,''),
                   FDBFILE.FDB_DOFFSET_72,
                   FDBFILE.FDB_DDIGITS_72,
                   FDBFILE.FDB_DTYPE_72,
                   FDBFILE.FDB_DDECIMAL_72,
                   ISNULL(FDBFILE.FDB_DNAME_73,''),
                   FDBFILE.FDB_DOFFSET_73,
                   FDBFILE.FDB_DDIGITS_73,
                   FDBFILE.FDB_DTYPE_73,
                   FDBFILE.FDB_DDECIMAL_73,
                   ISNULL(FDBFILE.FDB_DNAME_74,''),
                   FDBFILE.FDB_DOFFSET_74,
                   FDBFILE.FDB_DDIGITS_74,
                   FDBFILE.FDB_DTYPE_74,
                   FDBFILE.FDB_DDECIMAL_74,
                   ISNULL(FDBFILE.FDB_DNAME_75,''),
                   FDBFILE.FDB_DOFFSET_75,
                   FDBFILE.FDB_DDIGITS_75,
                   FDBFILE.FDB_DTYPE_75,
                   FDBFILE.FDB_DDECIMAL_75,
                   ISNULL(FDBFILE.FDB_DNAME_76,''),
                   FDBFILE.FDB_DOFFSET_76,
                   FDBFILE.FDB_DDIGITS_76,
                   FDBFILE.FDB_DTYPE_76,
                   FDBFILE.FDB_DDECIMAL_76,
                   ISNULL(FDBFILE.FDB_DNAME_77,''),
                   FDBFILE.FDB_DOFFSET_77,
                   FDBFILE.FDB_DDIGITS_77,
                   FDBFILE.FDB_DTYPE_77,
                   FDBFILE.FDB_DDECIMAL_77,
                   ISNULL(FDBFILE.FDB_DNAME_78,''),
                   FDBFILE.FDB_DOFFSET_78,
                   FDBFILE.FDB_DDIGITS_78,
                   FDBFILE.FDB_DTYPE_78,
                   FDBFILE.FDB_DDECIMAL_78,
                   ISNULL(FDBFILE.FDB_DNAME_79,''),
                   FDBFILE.FDB_DOFFSET_79,
                   FDBFILE.FDB_DDIGITS_79,
                   FDBFILE.FDB_DTYPE_79,
                   FDBFILE.FDB_DDECIMAL_79,
                   ISNULL(FDBFILE.FDB_DNAME_80,''),
                   FDBFILE.FDB_DOFFSET_80,
                   FDBFILE.FDB_DDIGITS_80,
                   FDBFILE.FDB_DTYPE_80,
                   FDBFILE.FDB_DDECIMAL_80,
                   ISNULL(FDBFILE.FDB_DNAME_81,''),
                   FDBFILE.FDB_DOFFSET_81,
                   FDBFILE.FDB_DDIGITS_81,
                   FDBFILE.FDB_DTYPE_81,
                   FDBFILE.FDB_DDECIMAL_81,
                   ISNULL(FDBFILE.FDB_DNAME_82,''),
                   FDBFILE.FDB_DOFFSET_82,
                   FDBFILE.FDB_DDIGITS_82,
                   FDBFILE.FDB_DTYPE_82,
                   FDBFILE.FDB_DDECIMAL_82,
                   ISNULL(FDBFILE.FDB_DNAME_83,''),
                   FDBFILE.FDB_DOFFSET_83,
                   FDBFILE.FDB_DDIGITS_83,
                   FDBFILE.FDB_DTYPE_83,
                   FDBFILE.FDB_DDECIMAL_83,
                   ISNULL(FDBFILE.FDB_DNAME_84,''),
                   FDBFILE.FDB_DOFFSET_84,
                   FDBFILE.FDB_DDIGITS_84,
                   FDBFILE.FDB_DTYPE_84,
                   FDBFILE.FDB_DDECIMAL_84,
                   ISNULL(FDBFILE.FDB_DNAME_85,''),
                   FDBFILE.FDB_DOFFSET_85,
                   FDBFILE.FDB_DDIGITS_85,
                   FDBFILE.FDB_DTYPE_85,
                   FDBFILE.FDB_DDECIMAL_85,
                   ISNULL(FDBFILE.FDB_DNAME_86,''),
                   FDBFILE.FDB_DOFFSET_86,
                   FDBFILE.FDB_DDIGITS_86,
                   FDBFILE.FDB_DTYPE_86,
                   FDBFILE.FDB_DDECIMAL_86,
                   ISNULL(FDBFILE.FDB_DNAME_87,''),
                   FDBFILE.FDB_DOFFSET_87,
                   FDBFILE.FDB_DDIGITS_87,
                   FDBFILE.FDB_DTYPE_87,
                   FDBFILE.FDB_DDECIMAL_87,
                   ISNULL(FDBFILE.FDB_DNAME_88,''),
                   FDBFILE.FDB_DOFFSET_88,
                   FDBFILE.FDB_DDIGITS_88,
                   FDBFILE.FDB_DTYPE_88,
                   FDBFILE.FDB_DDECIMAL_88,
                   ISNULL(FDBFILE.FDB_DNAME_89,''),
                   FDBFILE.FDB_DOFFSET_89,
                   FDBFILE.FDB_DDIGITS_89,
                   FDBFILE.FDB_DTYPE_89,
                   FDBFILE.FDB_DDECIMAL_89,
                   ISNULL(FDBFILE.FDB_DNAME_90,''),
                   FDBFILE.FDB_DOFFSET_90,
                   FDBFILE.FDB_DDIGITS_90,
                   FDBFILE.FDB_DTYPE_90,
                   FDBFILE.FDB_DDECIMAL_90,
                   ISNULL(FDBFILE.FDB_DNAME_91,''),
                   FDBFILE.FDB_DOFFSET_91,
                   FDBFILE.FDB_DDIGITS_91,
                   FDBFILE.FDB_DTYPE_91,
                   FDBFILE.FDB_DDECIMAL_91,
                   ISNULL(FDBFILE.FDB_DNAME_92,''),
                   FDBFILE.FDB_DOFFSET_92,
                   FDBFILE.FDB_DDIGITS_92,
                   FDBFILE.FDB_DTYPE_92,
                   FDBFILE.FDB_DDECIMAL_92,
                   ISNULL(FDBFILE.FDB_DNAME_93,''),
                   FDBFILE.FDB_DOFFSET_93,
                   FDBFILE.FDB_DDIGITS_93,
                   FDBFILE.FDB_DTYPE_93,
                   FDBFILE.FDB_DDECIMAL_93,
                   ISNULL(FDBFILE.FDB_DNAME_94,''),
                   FDBFILE.FDB_DOFFSET_94,
                   FDBFILE.FDB_DDIGITS_94,
                   FDBFILE.FDB_DTYPE_94,
                   FDBFILE.FDB_DDECIMAL_94,
                   ISNULL(FDBFILE.FDB_DNAME_95,''),
                   FDBFILE.FDB_DOFFSET_95,
                   FDBFILE.FDB_DDIGITS_95,
                   FDBFILE.FDB_DTYPE_95,
                   FDBFILE.FDB_DDECIMAL_95,
                   ISNULL(FDBFILE.FDB_DNAME_96,''),
                   FDBFILE.FDB_DOFFSET_96,
                   FDBFILE.FDB_DDIGITS_96,
                   FDBFILE.FDB_DTYPE_96,
                   FDBFILE.FDB_DDECIMAL_96,
                   ISNULL(FDBFILE.FDB_DNAME_97,''),
                   FDBFILE.FDB_DOFFSET_97,
                   FDBFILE.FDB_DDIGITS_97,
                   FDBFILE.FDB_DTYPE_97,
                   FDBFILE.FDB_DDECIMAL_97,
                   ISNULL(FDBFILE.FDB_DNAME_98,''),
                   FDBFILE.FDB_DOFFSET_98,
                   FDBFILE.FDB_DDIGITS_98,
                   FDBFILE.FDB_DTYPE_98,
                   FDBFILE.FDB_DDECIMAL_98,
                   ISNULL(FDBFILE.FDB_DNAME_99,''),
                   FDBFILE.FDB_DOFFSET_99,
                   FDBFILE.FDB_DDIGITS_99,
                   FDBFILE.FDB_DTYPE_99,
                   FDBFILE.FDB_DDECIMAL_99,
                   ISNULL(FDBFILE.FDB_DNAME_100,''),
                   FDBFILE.FDB_DOFFSET_100,
                   FDBFILE.FDB_DDIGITS_100,
                   FDBFILE.FDB_DTYPE_100,
                   FDBFILE.FDB_DDECIMAL_100,
                   ISNULL(FDBFILE.FDB_DNAME_101,''),
                   FDBFILE.FDB_DOFFSET_101,
                   FDBFILE.FDB_DDIGITS_101,
                   FDBFILE.FDB_DTYPE_101,
                   FDBFILE.FDB_DDECIMAL_101,
                   ISNULL(FDBFILE.FDB_DNAME_102,''),
                   FDBFILE.FDB_DOFFSET_102,
                   FDBFILE.FDB_DDIGITS_102,
                   FDBFILE.FDB_DTYPE_102,
                   FDBFILE.FDB_DDECIMAL_102,
                   ISNULL(FDBFILE.FDB_DNAME_103,''),
                   FDBFILE.FDB_DOFFSET_103,
                   FDBFILE.FDB_DDIGITS_103,
                   FDBFILE.FDB_DTYPE_103,
                   FDBFILE.FDB_DDECIMAL_103,
                   ISNULL(FDBFILE.FDB_DNAME_104,''),
                   FDBFILE.FDB_DOFFSET_104,
                   FDBFILE.FDB_DDIGITS_104,
                   FDBFILE.FDB_DTYPE_104,
                   FDBFILE.FDB_DDECIMAL_104,
                   ISNULL(FDBFILE.FDB_DNAME_105,''),
                   FDBFILE.FDB_DOFFSET_105,
                   FDBFILE.FDB_DDIGITS_105,
                   FDBFILE.FDB_DTYPE_105,
                   FDBFILE.FDB_DDECIMAL_105,
                   ISNULL(FDBFILE.FDB_DNAME_106,''),
                   FDBFILE.FDB_DOFFSET_106,
                   FDBFILE.FDB_DDIGITS_106,
                   FDBFILE.FDB_DTYPE_106,
                   FDBFILE.FDB_DDECIMAL_106,
                   ISNULL(FDBFILE.FDB_DNAME_107,''),
                   FDBFILE.FDB_DOFFSET_107,
                   FDBFILE.FDB_DDIGITS_107,
                   FDBFILE.FDB_DTYPE_107,
                   FDBFILE.FDB_DDECIMAL_107,
                   ISNULL(FDBFILE.FDB_DNAME_108,''),
                   FDBFILE.FDB_DOFFSET_108,
                   FDBFILE.FDB_DDIGITS_108,
                   FDBFILE.FDB_DTYPE_108,
                   FDBFILE.FDB_DDECIMAL_108,
                   ISNULL(FDBFILE.FDB_DNAME_109,''),
                   FDBFILE.FDB_DOFFSET_109,
                   FDBFILE.FDB_DDIGITS_109,
                   FDBFILE.FDB_DTYPE_109,
                   FDBFILE.FDB_DDECIMAL_109,
                   ISNULL(FDBFILE.FDB_DNAME_110,''),
                   FDBFILE.FDB_DOFFSET_110,
                   FDBFILE.FDB_DDIGITS_110,
                   FDBFILE.FDB_DTYPE_110,
                   FDBFILE.FDB_DDECIMAL_110,
                   ISNULL(FDBFILE.FDB_DNAME_111,''),
                   FDBFILE.FDB_DOFFSET_111,
                   FDBFILE.FDB_DDIGITS_111,
                   FDBFILE.FDB_DTYPE_111,
                   FDBFILE.FDB_DDECIMAL_111,
                   ISNULL(FDBFILE.FDB_DNAME_112,''),
                   FDBFILE.FDB_DOFFSET_112,
                   FDBFILE.FDB_DDIGITS_112,
                   FDBFILE.FDB_DTYPE_112,
                   FDBFILE.FDB_DDECIMAL_112,
                   ISNULL(FDBFILE.FDB_DNAME_113,''),
                   FDBFILE.FDB_DOFFSET_113,
                   FDBFILE.FDB_DDIGITS_113,
                   FDBFILE.FDB_DTYPE_113,
                   FDBFILE.FDB_DDECIMAL_113,
                   ISNULL(FDBFILE.FDB_DNAME_114,''),
                   FDBFILE.FDB_DOFFSET_114,
                   FDBFILE.FDB_DDIGITS_114,
                   FDBFILE.FDB_DTYPE_114,
                   FDBFILE.FDB_DDECIMAL_114,
                   ISNULL(FDBFILE.FDB_DNAME_115,''),
                   FDBFILE.FDB_DOFFSET_115,
                   FDBFILE.FDB_DDIGITS_115,
                   FDBFILE.FDB_DTYPE_115,
                   FDBFILE.FDB_DDECIMAL_115,
                   ISNULL(FDBFILE.FDB_DNAME_116,''),
                   FDBFILE.FDB_DOFFSET_116,
                   FDBFILE.FDB_DDIGITS_116,
                   FDBFILE.FDB_DTYPE_116,
                   FDBFILE.FDB_DDECIMAL_116,
                   ISNULL(FDBFILE.FDB_DNAME_117,''),
                   FDBFILE.FDB_DOFFSET_117,
                   FDBFILE.FDB_DDIGITS_117,
                   FDBFILE.FDB_DTYPE_117,
                   FDBFILE.FDB_DDECIMAL_117,
                   ISNULL(FDBFILE.FDB_DNAME_118,''),
                   FDBFILE.FDB_DOFFSET_118,
                   FDBFILE.FDB_DDIGITS_118,
                   FDBFILE.FDB_DTYPE_118,
                   FDBFILE.FDB_DDECIMAL_118,
                   ISNULL(FDBFILE.FDB_DNAME_119,''),
                   FDBFILE.FDB_DOFFSET_119,
                   FDBFILE.FDB_DDIGITS_119,
                   FDBFILE.FDB_DTYPE_119,
                   FDBFILE.FDB_DDECIMAL_119,
                   ISNULL(FDBFILE.FDB_DNAME_120,''),
                   FDBFILE.FDB_DOFFSET_120,
                   FDBFILE.FDB_DDIGITS_120,
                   FDBFILE.FDB_DTYPE_120,
                   FDBFILE.FDB_DDECIMAL_120,
                   ISNULL(FDBFILE.FDB_DNAME_121,''),
                   FDBFILE.FDB_DOFFSET_121,
                   FDBFILE.FDB_DDIGITS_121,
                   FDBFILE.FDB_DTYPE_121,
                   FDBFILE.FDB_DDECIMAL_121,
                   ISNULL(FDBFILE.FDB_DNAME_122,''),
                   FDBFILE.FDB_DOFFSET_122,
                   FDBFILE.FDB_DDIGITS_122,
                   FDBFILE.FDB_DTYPE_122,
                   FDBFILE.FDB_DDECIMAL_122,
                   ISNULL(FDBFILE.FDB_DNAME_123,''),
                   FDBFILE.FDB_DOFFSET_123,
                   FDBFILE.FDB_DDIGITS_123,
                   FDBFILE.FDB_DTYPE_123,
                   FDBFILE.FDB_DDECIMAL_123,
                   ISNULL(FDBFILE.FDB_DNAME_124,''),
                   FDBFILE.FDB_DOFFSET_124,
                   FDBFILE.FDB_DDIGITS_124,
                   FDBFILE.FDB_DTYPE_124,
                   FDBFILE.FDB_DDECIMAL_124,
                   ISNULL(FDBFILE.FDB_DNAME_125,''),
                   FDBFILE.FDB_DOFFSET_125,
                   FDBFILE.FDB_DDIGITS_125,
                   FDBFILE.FDB_DTYPE_125,
                   FDBFILE.FDB_DDECIMAL_125,
                   ISNULL(FDBFILE.FDB_DNAME_126,''),
                   FDBFILE.FDB_DOFFSET_126,
                   FDBFILE.FDB_DDIGITS_126,
                   FDBFILE.FDB_DTYPE_126,
                   FDBFILE.FDB_DDECIMAL_126,
                   ISNULL(FDBFILE.FDB_DNAME_127,''),
                   FDBFILE.FDB_DOFFSET_127,
                   FDBFILE.FDB_DDIGITS_127,
                   FDBFILE.FDB_DTYPE_127,
                   FDBFILE.FDB_DDECIMAL_127,
                   ISNULL(FDBFILE.FDB_DNAME_128,''),
                   FDBFILE.FDB_DOFFSET_128,
                   FDBFILE.FDB_DDIGITS_128,
                   FDBFILE.FDB_DTYPE_128,
                   FDBFILE.FDB_DDECIMAL_128,
                   ISNULL(FDBFILE.FDB_DNAME_129,''),
                   FDBFILE.FDB_DOFFSET_129,
                   FDBFILE.FDB_DDIGITS_129,
                   FDBFILE.FDB_DTYPE_129,
                   FDBFILE.FDB_DDECIMAL_129,
                   ISNULL(FDBFILE.FDB_DNAME_130,''),
                   FDBFILE.FDB_DOFFSET_130,
                   FDBFILE.FDB_DDIGITS_130,
                   FDBFILE.FDB_DTYPE_130,
                   FDBFILE.FDB_DDECIMAL_130,
                   ISNULL(FDBFILE.FDB_DNAME_131,''),
                   FDBFILE.FDB_DOFFSET_131,
                   FDBFILE.FDB_DDIGITS_131,
                   FDBFILE.FDB_DTYPE_131,
                   FDBFILE.FDB_DDECIMAL_131,
                   ISNULL(FDBFILE.FDB_DNAME_132,''),
                   FDBFILE.FDB_DOFFSET_132,
                   FDBFILE.FDB_DDIGITS_132,
                   FDBFILE.FDB_DTYPE_132,
                   FDBFILE.FDB_DDECIMAL_132,
                   ISNULL(FDBFILE.FDB_DNAME_133,''),
                   FDBFILE.FDB_DOFFSET_133,
                   FDBFILE.FDB_DDIGITS_133,
                   FDBFILE.FDB_DTYPE_133,
                   FDBFILE.FDB_DDECIMAL_133,
                   ISNULL(FDBFILE.FDB_DNAME_134,''),
                   FDBFILE.FDB_DOFFSET_134,
                   FDBFILE.FDB_DDIGITS_134,
                   FDBFILE.FDB_DTYPE_134,
                   FDBFILE.FDB_DDECIMAL_134,
                   ISNULL(FDBFILE.FDB_DNAME_135,''),
                   FDBFILE.FDB_DOFFSET_135,
                   FDBFILE.FDB_DDIGITS_135,
                   FDBFILE.FDB_DTYPE_135,
                   FDBFILE.FDB_DDECIMAL_135,
                   ISNULL(FDBFILE.FDB_DNAME_136,''),
                   FDBFILE.FDB_DOFFSET_136,
                   FDBFILE.FDB_DDIGITS_136,
                   FDBFILE.FDB_DTYPE_136,
                   FDBFILE.FDB_DDECIMAL_136,
                   ISNULL(FDBFILE.FDB_DNAME_137,''),
                   FDBFILE.FDB_DOFFSET_137,
                   FDBFILE.FDB_DDIGITS_137,
                   FDBFILE.FDB_DTYPE_137,
                   FDBFILE.FDB_DDECIMAL_137,
                   ISNULL(FDBFILE.FDB_DNAME_138,''),
                   FDBFILE.FDB_DOFFSET_138,
                   FDBFILE.FDB_DDIGITS_138,
                   FDBFILE.FDB_DTYPE_138,
                   FDBFILE.FDB_DDECIMAL_138,
                   ISNULL(FDBFILE.FDB_DNAME_139,''),
                   FDBFILE.FDB_DOFFSET_139,
                   FDBFILE.FDB_DDIGITS_139,
                   FDBFILE.FDB_DTYPE_139,
                   FDBFILE.FDB_DDECIMAL_139,
                   ISNULL(FDBFILE.FDB_DNAME_140,''),
                   FDBFILE.FDB_DOFFSET_140,
                   FDBFILE.FDB_DDIGITS_140,
                   FDBFILE.FDB_DTYPE_140,
                   FDBFILE.FDB_DDECIMAL_140,
                   ISNULL(FDBFILE.FDB_DNAME_141,''),
                   FDBFILE.FDB_DOFFSET_141,
                   FDBFILE.FDB_DDIGITS_141,
                   FDBFILE.FDB_DTYPE_141,
                   FDBFILE.FDB_DDECIMAL_141,
                   ISNULL(FDBFILE.FDB_DNAME_142,''),
                   FDBFILE.FDB_DOFFSET_142,
                   FDBFILE.FDB_DDIGITS_142,
                   FDBFILE.FDB_DTYPE_142,
                   FDBFILE.FDB_DDECIMAL_142,
                   ISNULL(FDBFILE.FDB_DNAME_143,''),
                   FDBFILE.FDB_DOFFSET_143,
                   FDBFILE.FDB_DDIGITS_143,
                   FDBFILE.FDB_DTYPE_143,
                   FDBFILE.FDB_DDECIMAL_143,
                   ISNULL(FDBFILE.FDB_DNAME_144,''),
                   FDBFILE.FDB_DOFFSET_144,
                   FDBFILE.FDB_DDIGITS_144,
                   FDBFILE.FDB_DTYPE_144,
                   FDBFILE.FDB_DDECIMAL_144,
                   ISNULL(FDBFILE.FDB_DNAME_145,''),
                   FDBFILE.FDB_DOFFSET_145,
                   FDBFILE.FDB_DDIGITS_145,
                   FDBFILE.FDB_DTYPE_145,
                   FDBFILE.FDB_DDECIMAL_145,
                   ISNULL(FDBFILE.FDB_DNAME_146,''),
                   FDBFILE.FDB_DOFFSET_146,
                   FDBFILE.FDB_DDIGITS_146,
                   FDBFILE.FDB_DTYPE_146,
                   FDBFILE.FDB_DDECIMAL_146,
                   ISNULL(FDBFILE.FDB_DNAME_147,''),
                   FDBFILE.FDB_DOFFSET_147,
                   FDBFILE.FDB_DDIGITS_147,
                   FDBFILE.FDB_DTYPE_147,
                   FDBFILE.FDB_DDECIMAL_147,
                   ISNULL(FDBFILE.FDB_DNAME_148,''),
                   FDBFILE.FDB_DOFFSET_148,
                   FDBFILE.FDB_DDIGITS_148,
                   FDBFILE.FDB_DTYPE_148,
                   FDBFILE.FDB_DDECIMAL_148,
                   ISNULL(FDBFILE.FDB_DNAME_149,''),
                   FDBFILE.FDB_DOFFSET_149,
                   FDBFILE.FDB_DDIGITS_149,
                   FDBFILE.FDB_DTYPE_149,
                   FDBFILE.FDB_DDECIMAL_149,
                   ISNULL(FDBFILE.FDB_DNAME_150,''),
                   FDBFILE.FDB_DOFFSET_150,
                   FDBFILE.FDB_DDIGITS_150,
                   FDBFILE.FDB_DTYPE_150,
                   FDBFILE.FDB_DDECIMAL_150,
                   ISNULL(FDBFILE.FDB_DNAME_151,''),
                   FDBFILE.FDB_DOFFSET_151,
                   FDBFILE.FDB_DDIGITS_151,
                   FDBFILE.FDB_DTYPE_151,
                   FDBFILE.FDB_DDECIMAL_151,
                   ISNULL(FDBFILE.FDB_DNAME_152,''),
                   FDBFILE.FDB_DOFFSET_152,
                   FDBFILE.FDB_DDIGITS_152,
                   FDBFILE.FDB_DTYPE_152,
                   FDBFILE.FDB_DDECIMAL_152,
                   ISNULL(FDBFILE.FDB_DNAME_153,''),
                   FDBFILE.FDB_DOFFSET_153,
                   FDBFILE.FDB_DDIGITS_153,
                   FDBFILE.FDB_DTYPE_153,
                   FDBFILE.FDB_DDECIMAL_153,
                   ISNULL(FDBFILE.FDB_DNAME_154,''),
                   FDBFILE.FDB_DOFFSET_154,
                   FDBFILE.FDB_DDIGITS_154,
                   FDBFILE.FDB_DTYPE_154,
                   FDBFILE.FDB_DDECIMAL_154,
                   ISNULL(FDBFILE.FDB_DNAME_155,''),
                   FDBFILE.FDB_DOFFSET_155,
                   FDBFILE.FDB_DDIGITS_155,
                   FDBFILE.FDB_DTYPE_155,
                   FDBFILE.FDB_DDECIMAL_155,
                   ISNULL(FDBFILE.FDB_DNAME_156,''),
                   FDBFILE.FDB_DOFFSET_156,
                   FDBFILE.FDB_DDIGITS_156,
                   FDBFILE.FDB_DTYPE_156,
                   FDBFILE.FDB_DDECIMAL_156,
                   ISNULL(FDBFILE.FDB_DNAME_157,''),
                   FDBFILE.FDB_DOFFSET_157,
                   FDBFILE.FDB_DDIGITS_157,
                   FDBFILE.FDB_DTYPE_157,
                   FDBFILE.FDB_DDECIMAL_157,
                   ISNULL(FDBFILE.FDB_DNAME_158,''),
                   FDBFILE.FDB_DOFFSET_158,
                   FDBFILE.FDB_DDIGITS_158,
                   FDBFILE.FDB_DTYPE_158,
                   FDBFILE.FDB_DDECIMAL_158,
                   ISNULL(FDBFILE.FDB_DNAME_159,''),
                   FDBFILE.FDB_DOFFSET_159,
                   FDBFILE.FDB_DDIGITS_159,
                   FDBFILE.FDB_DTYPE_159,
                   FDBFILE.FDB_DDECIMAL_159,
                   ISNULL(FDBFILE.FDB_DNAME_160,''),
                   FDBFILE.FDB_DOFFSET_160,
                   FDBFILE.FDB_DDIGITS_160,
                   FDBFILE.FDB_DTYPE_160,
                   FDBFILE.FDB_DDECIMAL_160,
                   ISNULL(FDBFILE.FDB_DNAME_161,''),
                   FDBFILE.FDB_DOFFSET_161,
                   FDBFILE.FDB_DDIGITS_161,
                   FDBFILE.FDB_DTYPE_161,
                   FDBFILE.FDB_DDECIMAL_161,
                   ISNULL(FDBFILE.FDB_DNAME_162,''),
                   FDBFILE.FDB_DOFFSET_162,
                   FDBFILE.FDB_DDIGITS_162,
                   FDBFILE.FDB_DTYPE_162,
                   FDBFILE.FDB_DDECIMAL_162,
                   ISNULL(FDBFILE.FDB_DNAME_163,''),
                   FDBFILE.FDB_DOFFSET_163,
                   FDBFILE.FDB_DDIGITS_163,
                   FDBFILE.FDB_DTYPE_163,
                   FDBFILE.FDB_DDECIMAL_163,
                   ISNULL(FDBFILE.FDB_DNAME_164,''),
                   FDBFILE.FDB_DOFFSET_164,
                   FDBFILE.FDB_DDIGITS_164,
                   FDBFILE.FDB_DTYPE_164,
                   FDBFILE.FDB_DDECIMAL_164,
                   ISNULL(FDBFILE.FDB_DNAME_165,''),
                   FDBFILE.FDB_DOFFSET_165,
                   FDBFILE.FDB_DDIGITS_165,
                   FDBFILE.FDB_DTYPE_165,
                   FDBFILE.FDB_DDECIMAL_165,
                   ISNULL(FDBFILE.FDB_DNAME_166,''),
                   FDBFILE.FDB_DOFFSET_166,
                   FDBFILE.FDB_DDIGITS_166,
                   FDBFILE.FDB_DTYPE_166,
                   FDBFILE.FDB_DDECIMAL_166,
                   ISNULL(FDBFILE.FDB_DNAME_167,''),
                   FDBFILE.FDB_DOFFSET_167,
                   FDBFILE.FDB_DDIGITS_167,
                   FDBFILE.FDB_DTYPE_167,
                   FDBFILE.FDB_DDECIMAL_167,
                   ISNULL(FDBFILE.FDB_DNAME_168,''),
                   FDBFILE.FDB_DOFFSET_168,
                   FDBFILE.FDB_DDIGITS_168,
                   FDBFILE.FDB_DTYPE_168,
                   FDBFILE.FDB_DDECIMAL_168,
                   ISNULL(FDBFILE.FDB_DNAME_169,''),
                   FDBFILE.FDB_DOFFSET_169,
                   FDBFILE.FDB_DDIGITS_169,
                   FDBFILE.FDB_DTYPE_169,
                   FDBFILE.FDB_DDECIMAL_169,
                   ISNULL(FDBFILE.FDB_DNAME_170,''),
                   FDBFILE.FDB_DOFFSET_170,
                   FDBFILE.FDB_DDIGITS_170,
                   FDBFILE.FDB_DTYPE_170,
                   FDBFILE.FDB_DDECIMAL_170,
                   ISNULL(FDBFILE.FDB_DNAME_171,''),
                   FDBFILE.FDB_DOFFSET_171,
                   FDBFILE.FDB_DDIGITS_171,
                   FDBFILE.FDB_DTYPE_171,
                   FDBFILE.FDB_DDECIMAL_171,
                   ISNULL(FDBFILE.FDB_DNAME_172,''),
                   FDBFILE.FDB_DOFFSET_172,
                   FDBFILE.FDB_DDIGITS_172,
                   FDBFILE.FDB_DTYPE_172,
                   FDBFILE.FDB_DDECIMAL_172,
                   ISNULL(FDBFILE.FDB_DNAME_173,''),
                   FDBFILE.FDB_DOFFSET_173,
                   FDBFILE.FDB_DDIGITS_173,
                   FDBFILE.FDB_DTYPE_173,
                   FDBFILE.FDB_DDECIMAL_173,
                   ISNULL(FDBFILE.FDB_DNAME_174,''),
                   FDBFILE.FDB_DOFFSET_174,
                   FDBFILE.FDB_DDIGITS_174,
                   FDBFILE.FDB_DTYPE_174,
                   FDBFILE.FDB_DDECIMAL_174,
                   ISNULL(FDBFILE.FDB_DNAME_175,''),
                   FDBFILE.FDB_DOFFSET_175,
                   FDBFILE.FDB_DDIGITS_175,
                   FDBFILE.FDB_DTYPE_175,
                   FDBFILE.FDB_DDECIMAL_175,
                   ISNULL(FDBFILE.FDB_DNAME_176,''),
                   FDBFILE.FDB_DOFFSET_176,
                   FDBFILE.FDB_DDIGITS_176,
                   FDBFILE.FDB_DTYPE_176,
                   FDBFILE.FDB_DDECIMAL_176,
                   ISNULL(FDBFILE.FDB_DNAME_177,''),
                   FDBFILE.FDB_DOFFSET_177,
                   FDBFILE.FDB_DDIGITS_177,
                   FDBFILE.FDB_DTYPE_177,
                   FDBFILE.FDB_DDECIMAL_177,
                   ISNULL(FDBFILE.FDB_DNAME_178,''),
                   FDBFILE.FDB_DOFFSET_178,
                   FDBFILE.FDB_DDIGITS_178,
                   FDBFILE.FDB_DTYPE_178,
                   FDBFILE.FDB_DDECIMAL_178,
                   ISNULL(FDBFILE.FDB_DNAME_179,''),
                   FDBFILE.FDB_DOFFSET_179,
                   FDBFILE.FDB_DDIGITS_179,
                   FDBFILE.FDB_DTYPE_179,
                   FDBFILE.FDB_DDECIMAL_179,
                   ISNULL(FDBFILE.FDB_DNAME_180,''),
                   FDBFILE.FDB_DOFFSET_180,
                   FDBFILE.FDB_DDIGITS_180,
                   FDBFILE.FDB_DTYPE_180,
                   FDBFILE.FDB_DDECIMAL_180,
                   ISNULL(FDBFILE.FDB_DNAME_181,''),
                   FDBFILE.FDB_DOFFSET_181,
                   FDBFILE.FDB_DDIGITS_181,
                   FDBFILE.FDB_DTYPE_181,
                   FDBFILE.FDB_DDECIMAL_181,
                   ISNULL(FDBFILE.FDB_DNAME_182,''),
                   FDBFILE.FDB_DOFFSET_182,
                   FDBFILE.FDB_DDIGITS_182,
                   FDBFILE.FDB_DTYPE_182,
                   FDBFILE.FDB_DDECIMAL_182,
                   ISNULL(FDBFILE.FDB_DNAME_183,''),
                   FDBFILE.FDB_DOFFSET_183,
                   FDBFILE.FDB_DDIGITS_183,
                   FDBFILE.FDB_DTYPE_183,
                   FDBFILE.FDB_DDECIMAL_183,
                   ISNULL(FDBFILE.FDB_DNAME_184,''),
                   FDBFILE.FDB_DOFFSET_184,
                   FDBFILE.FDB_DDIGITS_184,
                   FDBFILE.FDB_DTYPE_184,
                   FDBFILE.FDB_DDECIMAL_184,
                   ISNULL(FDBFILE.FDB_DNAME_185,''),
                   FDBFILE.FDB_DOFFSET_185,
                   FDBFILE.FDB_DDIGITS_185,
                   FDBFILE.FDB_DTYPE_185,
                   FDBFILE.FDB_DDECIMAL_185,
                   ISNULL(FDBFILE.FDB_DNAME_186,''),
                   FDBFILE.FDB_DOFFSET_186,
                   FDBFILE.FDB_DDIGITS_186,
                   FDBFILE.FDB_DTYPE_186,
                   FDBFILE.FDB_DDECIMAL_186,
                   ISNULL(FDBFILE.FDB_DNAME_187,''),
                   FDBFILE.FDB_DOFFSET_187,
                   FDBFILE.FDB_DDIGITS_187,
                   FDBFILE.FDB_DTYPE_187,
                   FDBFILE.FDB_DDECIMAL_187,
                   ISNULL(FDBFILE.FDB_DNAME_188,''),
                   FDBFILE.FDB_DOFFSET_188,
                   FDBFILE.FDB_DDIGITS_188,
                   FDBFILE.FDB_DTYPE_188,
                   FDBFILE.FDB_DDECIMAL_188,
                   ISNULL(FDBFILE.FDB_DNAME_189,''),
                   FDBFILE.FDB_DOFFSET_189,
                   FDBFILE.FDB_DDIGITS_189,
                   FDBFILE.FDB_DTYPE_189,
                   FDBFILE.FDB_DDECIMAL_189,
                   ISNULL(FDBFILE.FDB_DNAME_190,''),
                   FDBFILE.FDB_DOFFSET_190,
                   FDBFILE.FDB_DDIGITS_190,
                   FDBFILE.FDB_DTYPE_190,
                   FDBFILE.FDB_DDECIMAL_190,
                   ISNULL(FDBFILE.FDB_DNAME_191,''),
                   FDBFILE.FDB_DOFFSET_191,
                   FDBFILE.FDB_DDIGITS_191,
                   FDBFILE.FDB_DTYPE_191,
                   FDBFILE.FDB_DDECIMAL_191,
                   ISNULL(FDBFILE.FDB_DNAME_192,''),
                   FDBFILE.FDB_DOFFSET_192,
                   FDBFILE.FDB_DDIGITS_192,
                   FDBFILE.FDB_DTYPE_192,
                   FDBFILE.FDB_DDECIMAL_192,
                   ISNULL(FDBFILE.FDB_DNAME_193,''),
                   FDBFILE.FDB_DOFFSET_193,
                   FDBFILE.FDB_DDIGITS_193,
                   FDBFILE.FDB_DTYPE_193,
                   FDBFILE.FDB_DDECIMAL_193,
                   ISNULL(FDBFILE.FDB_DNAME_194,''),
                   FDBFILE.FDB_DOFFSET_194,
                   FDBFILE.FDB_DDIGITS_194,
                   FDBFILE.FDB_DTYPE_194,
                   FDBFILE.FDB_DDECIMAL_194,
                   ISNULL(FDBFILE.FDB_DNAME_195,''),
                   FDBFILE.FDB_DOFFSET_195,
                   FDBFILE.FDB_DDIGITS_195,
                   FDBFILE.FDB_DTYPE_195,
                   FDBFILE.FDB_DDECIMAL_195,
                   ISNULL(FDBFILE.FDB_DNAME_196,''),
                   FDBFILE.FDB_DOFFSET_196,
                   FDBFILE.FDB_DDIGITS_196,
                   FDBFILE.FDB_DTYPE_196,
                   FDBFILE.FDB_DDECIMAL_196,
                   ISNULL(FDBFILE.FDB_DNAME_197,''),
                   FDBFILE.FDB_DOFFSET_197,
                   FDBFILE.FDB_DDIGITS_197,
                   FDBFILE.FDB_DTYPE_197,
                   FDBFILE.FDB_DDECIMAL_197,
                   ISNULL(FDBFILE.FDB_DNAME_198,''),
                   FDBFILE.FDB_DOFFSET_198,
                   FDBFILE.FDB_DDIGITS_198,
                   FDBFILE.FDB_DTYPE_198,
                   FDBFILE.FDB_DDECIMAL_198,
                   ISNULL(FDBFILE.FDB_DNAME_199,''),
                   FDBFILE.FDB_DOFFSET_199,
                   FDBFILE.FDB_DDIGITS_199,
                   FDBFILE.FDB_DTYPE_199,
                   FDBFILE.FDB_DDECIMAL_199,
                   ISNULL(FDBFILE.FDB_DNAME_200,''),
                   FDBFILE.FDB_DOFFSET_200,
                   FDBFILE.FDB_DDIGITS_200,
                   FDBFILE.FDB_DTYPE_200,
                   FDBFILE.FDB_DDECIMAL_200,
                   ISNULL(FDBFILE.FDB_CALLID_1,''),
                   FDBFILE.FDB_KEYLOC_1,
                   ISNULL(FDBFILE.FDB_CALLID_2,''),
                   FDBFILE.FDB_KEYLOC_2
            INTO :QFDB-REC
            FROM DBO.FDBFILE
            WHERE FDBFILE.FDB_DBNO = :QFDB-DBNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FDB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FDB-FIELDS.

      *-----------------------------------------------------------------
       READ-FDB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.
           INITIALIZE QFDB-REC.

           IF ( FDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH FDB1_CURSOR INTO :QFDB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FDB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FDB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-FDB1-FILE.

           ACCEPT FDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.
           PERFORM SET-FDB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.FDBFILE
             (FDBFILE.FDB_DBNO,
              FDBFILE.FDB_LTOUCH_DATE,
              FDBFILE.FDB_DNAME_1,
              FDBFILE.FDB_DOFFSET_1,
              FDBFILE.FDB_DDIGITS_1,
              FDBFILE.FDB_DTYPE_1,
              FDBFILE.FDB_DDECIMAL_1,
              FDBFILE.FDB_DNAME_2,
              FDBFILE.FDB_DOFFSET_2,
              FDBFILE.FDB_DDIGITS_2,
              FDBFILE.FDB_DTYPE_2,
              FDBFILE.FDB_DDECIMAL_2,
              FDBFILE.FDB_DNAME_3,
              FDBFILE.FDB_DOFFSET_3,
              FDBFILE.FDB_DDIGITS_3,
              FDBFILE.FDB_DTYPE_3,
              FDBFILE.FDB_DDECIMAL_3,
              FDBFILE.FDB_DNAME_4,
              FDBFILE.FDB_DOFFSET_4,
              FDBFILE.FDB_DDIGITS_4,
              FDBFILE.FDB_DTYPE_4,
              FDBFILE.FDB_DDECIMAL_4,
              FDBFILE.FDB_DNAME_5,
              FDBFILE.FDB_DOFFSET_5,
              FDBFILE.FDB_DDIGITS_5,
              FDBFILE.FDB_DTYPE_5,
              FDBFILE.FDB_DDECIMAL_5,
              FDBFILE.FDB_DNAME_6,
              FDBFILE.FDB_DOFFSET_6,
              FDBFILE.FDB_DDIGITS_6,
              FDBFILE.FDB_DTYPE_6,
              FDBFILE.FDB_DDECIMAL_6,
              FDBFILE.FDB_DNAME_7,
              FDBFILE.FDB_DOFFSET_7,
              FDBFILE.FDB_DDIGITS_7,
              FDBFILE.FDB_DTYPE_7,
              FDBFILE.FDB_DDECIMAL_7,
              FDBFILE.FDB_DNAME_8,
              FDBFILE.FDB_DOFFSET_8,
              FDBFILE.FDB_DDIGITS_8,
              FDBFILE.FDB_DTYPE_8,
              FDBFILE.FDB_DDECIMAL_8,
              FDBFILE.FDB_DNAME_9,
              FDBFILE.FDB_DOFFSET_9,
              FDBFILE.FDB_DDIGITS_9,
              FDBFILE.FDB_DTYPE_9,
              FDBFILE.FDB_DDECIMAL_9,
              FDBFILE.FDB_DNAME_10,
              FDBFILE.FDB_DOFFSET_10,
              FDBFILE.FDB_DDIGITS_10,
              FDBFILE.FDB_DTYPE_10,
              FDBFILE.FDB_DDECIMAL_10,
              FDBFILE.FDB_DNAME_11,
              FDBFILE.FDB_DOFFSET_11,
              FDBFILE.FDB_DDIGITS_11,
              FDBFILE.FDB_DTYPE_11,
              FDBFILE.FDB_DDECIMAL_11,
              FDBFILE.FDB_DNAME_12,
              FDBFILE.FDB_DOFFSET_12,
              FDBFILE.FDB_DDIGITS_12,
              FDBFILE.FDB_DTYPE_12,
              FDBFILE.FDB_DDECIMAL_12,
              FDBFILE.FDB_DNAME_13,
              FDBFILE.FDB_DOFFSET_13,
              FDBFILE.FDB_DDIGITS_13,
              FDBFILE.FDB_DTYPE_13,
              FDBFILE.FDB_DDECIMAL_13,
              FDBFILE.FDB_DNAME_14,
              FDBFILE.FDB_DOFFSET_14,
              FDBFILE.FDB_DDIGITS_14,
              FDBFILE.FDB_DTYPE_14,
              FDBFILE.FDB_DDECIMAL_14,
              FDBFILE.FDB_DNAME_15,
              FDBFILE.FDB_DOFFSET_15,
              FDBFILE.FDB_DDIGITS_15,
              FDBFILE.FDB_DTYPE_15,
              FDBFILE.FDB_DDECIMAL_15,
              FDBFILE.FDB_DNAME_16,
              FDBFILE.FDB_DOFFSET_16,
              FDBFILE.FDB_DDIGITS_16,
              FDBFILE.FDB_DTYPE_16,
              FDBFILE.FDB_DDECIMAL_16,
              FDBFILE.FDB_DNAME_17,
              FDBFILE.FDB_DOFFSET_17,
              FDBFILE.FDB_DDIGITS_17,
              FDBFILE.FDB_DTYPE_17,
              FDBFILE.FDB_DDECIMAL_17,
              FDBFILE.FDB_DNAME_18,
              FDBFILE.FDB_DOFFSET_18,
              FDBFILE.FDB_DDIGITS_18,
              FDBFILE.FDB_DTYPE_18,
              FDBFILE.FDB_DDECIMAL_18,
              FDBFILE.FDB_DNAME_19,
              FDBFILE.FDB_DOFFSET_19,
              FDBFILE.FDB_DDIGITS_19,
              FDBFILE.FDB_DTYPE_19,
              FDBFILE.FDB_DDECIMAL_19,
              FDBFILE.FDB_DNAME_20,
              FDBFILE.FDB_DOFFSET_20,
              FDBFILE.FDB_DDIGITS_20,
              FDBFILE.FDB_DTYPE_20,
              FDBFILE.FDB_DDECIMAL_20,
              FDBFILE.FDB_DNAME_21,
              FDBFILE.FDB_DOFFSET_21,
              FDBFILE.FDB_DDIGITS_21,
              FDBFILE.FDB_DTYPE_21,
              FDBFILE.FDB_DDECIMAL_21,
              FDBFILE.FDB_DNAME_22,
              FDBFILE.FDB_DOFFSET_22,
              FDBFILE.FDB_DDIGITS_22,
              FDBFILE.FDB_DTYPE_22,
              FDBFILE.FDB_DDECIMAL_22,
              FDBFILE.FDB_DNAME_23,
              FDBFILE.FDB_DOFFSET_23,
              FDBFILE.FDB_DDIGITS_23,
              FDBFILE.FDB_DTYPE_23,
              FDBFILE.FDB_DDECIMAL_23,
              FDBFILE.FDB_DNAME_24,
              FDBFILE.FDB_DOFFSET_24,
              FDBFILE.FDB_DDIGITS_24,
              FDBFILE.FDB_DTYPE_24,
              FDBFILE.FDB_DDECIMAL_24,
              FDBFILE.FDB_DNAME_25,
              FDBFILE.FDB_DOFFSET_25,
              FDBFILE.FDB_DDIGITS_25,
              FDBFILE.FDB_DTYPE_25,
              FDBFILE.FDB_DDECIMAL_25,
              FDBFILE.FDB_DNAME_26,
              FDBFILE.FDB_DOFFSET_26,
              FDBFILE.FDB_DDIGITS_26,
              FDBFILE.FDB_DTYPE_26,
              FDBFILE.FDB_DDECIMAL_26,
              FDBFILE.FDB_DNAME_27,
              FDBFILE.FDB_DOFFSET_27,
              FDBFILE.FDB_DDIGITS_27,
              FDBFILE.FDB_DTYPE_27,
              FDBFILE.FDB_DDECIMAL_27,
              FDBFILE.FDB_DNAME_28,
              FDBFILE.FDB_DOFFSET_28,
              FDBFILE.FDB_DDIGITS_28,
              FDBFILE.FDB_DTYPE_28,
              FDBFILE.FDB_DDECIMAL_28,
              FDBFILE.FDB_DNAME_29,
              FDBFILE.FDB_DOFFSET_29,
              FDBFILE.FDB_DDIGITS_29,
              FDBFILE.FDB_DTYPE_29,
              FDBFILE.FDB_DDECIMAL_29,
              FDBFILE.FDB_DNAME_30,
              FDBFILE.FDB_DOFFSET_30,
              FDBFILE.FDB_DDIGITS_30,
              FDBFILE.FDB_DTYPE_30,
              FDBFILE.FDB_DDECIMAL_30,
              FDBFILE.FDB_DNAME_31,
              FDBFILE.FDB_DOFFSET_31,
              FDBFILE.FDB_DDIGITS_31,
              FDBFILE.FDB_DTYPE_31,
              FDBFILE.FDB_DDECIMAL_31,
              FDBFILE.FDB_DNAME_32,
              FDBFILE.FDB_DOFFSET_32,
              FDBFILE.FDB_DDIGITS_32,
              FDBFILE.FDB_DTYPE_32,
              FDBFILE.FDB_DDECIMAL_32,
              FDBFILE.FDB_DNAME_33,
              FDBFILE.FDB_DOFFSET_33,
              FDBFILE.FDB_DDIGITS_33,
              FDBFILE.FDB_DTYPE_33,
              FDBFILE.FDB_DDECIMAL_33,
              FDBFILE.FDB_DNAME_34,
              FDBFILE.FDB_DOFFSET_34,
              FDBFILE.FDB_DDIGITS_34,
              FDBFILE.FDB_DTYPE_34,
              FDBFILE.FDB_DDECIMAL_34,
              FDBFILE.FDB_DNAME_35,
              FDBFILE.FDB_DOFFSET_35,
              FDBFILE.FDB_DDIGITS_35,
              FDBFILE.FDB_DTYPE_35,
              FDBFILE.FDB_DDECIMAL_35,
              FDBFILE.FDB_DNAME_36,
              FDBFILE.FDB_DOFFSET_36,
              FDBFILE.FDB_DDIGITS_36,
              FDBFILE.FDB_DTYPE_36,
              FDBFILE.FDB_DDECIMAL_36,
              FDBFILE.FDB_DNAME_37,
              FDBFILE.FDB_DOFFSET_37,
              FDBFILE.FDB_DDIGITS_37,
              FDBFILE.FDB_DTYPE_37,
              FDBFILE.FDB_DDECIMAL_37,
              FDBFILE.FDB_DNAME_38,
              FDBFILE.FDB_DOFFSET_38,
              FDBFILE.FDB_DDIGITS_38,
              FDBFILE.FDB_DTYPE_38,
              FDBFILE.FDB_DDECIMAL_38,
              FDBFILE.FDB_DNAME_39,
              FDBFILE.FDB_DOFFSET_39,
              FDBFILE.FDB_DDIGITS_39,
              FDBFILE.FDB_DTYPE_39,
              FDBFILE.FDB_DDECIMAL_39,
              FDBFILE.FDB_DNAME_40,
              FDBFILE.FDB_DOFFSET_40,
              FDBFILE.FDB_DDIGITS_40,
              FDBFILE.FDB_DTYPE_40,
              FDBFILE.FDB_DDECIMAL_40,
              FDBFILE.FDB_DNAME_41,
              FDBFILE.FDB_DOFFSET_41,
              FDBFILE.FDB_DDIGITS_41,
              FDBFILE.FDB_DTYPE_41,
              FDBFILE.FDB_DDECIMAL_41,
              FDBFILE.FDB_DNAME_42,
              FDBFILE.FDB_DOFFSET_42,
              FDBFILE.FDB_DDIGITS_42,
              FDBFILE.FDB_DTYPE_42,
              FDBFILE.FDB_DDECIMAL_42,
              FDBFILE.FDB_DNAME_43,
              FDBFILE.FDB_DOFFSET_43,
              FDBFILE.FDB_DDIGITS_43,
              FDBFILE.FDB_DTYPE_43,
              FDBFILE.FDB_DDECIMAL_43,
              FDBFILE.FDB_DNAME_44,
              FDBFILE.FDB_DOFFSET_44,
              FDBFILE.FDB_DDIGITS_44,
              FDBFILE.FDB_DTYPE_44,
              FDBFILE.FDB_DDECIMAL_44,
              FDBFILE.FDB_DNAME_45,
              FDBFILE.FDB_DOFFSET_45,
              FDBFILE.FDB_DDIGITS_45,
              FDBFILE.FDB_DTYPE_45,
              FDBFILE.FDB_DDECIMAL_45,
              FDBFILE.FDB_DNAME_46,
              FDBFILE.FDB_DOFFSET_46,
              FDBFILE.FDB_DDIGITS_46,
              FDBFILE.FDB_DTYPE_46,
              FDBFILE.FDB_DDECIMAL_46,
              FDBFILE.FDB_DNAME_47,
              FDBFILE.FDB_DOFFSET_47,
              FDBFILE.FDB_DDIGITS_47,
              FDBFILE.FDB_DTYPE_47,
              FDBFILE.FDB_DDECIMAL_47,
              FDBFILE.FDB_DNAME_48,
              FDBFILE.FDB_DOFFSET_48,
              FDBFILE.FDB_DDIGITS_48,
              FDBFILE.FDB_DTYPE_48,
              FDBFILE.FDB_DDECIMAL_48,
              FDBFILE.FDB_DNAME_49,
              FDBFILE.FDB_DOFFSET_49,
              FDBFILE.FDB_DDIGITS_49,
              FDBFILE.FDB_DTYPE_49,
              FDBFILE.FDB_DDECIMAL_49,
              FDBFILE.FDB_DNAME_50,
              FDBFILE.FDB_DOFFSET_50,
              FDBFILE.FDB_DDIGITS_50,
              FDBFILE.FDB_DTYPE_50,
              FDBFILE.FDB_DDECIMAL_50,
              FDBFILE.FDB_DNAME_51,
              FDBFILE.FDB_DOFFSET_51,
              FDBFILE.FDB_DDIGITS_51,
              FDBFILE.FDB_DTYPE_51,
              FDBFILE.FDB_DDECIMAL_51,
              FDBFILE.FDB_DNAME_52,
              FDBFILE.FDB_DOFFSET_52,
              FDBFILE.FDB_DDIGITS_52,
              FDBFILE.FDB_DTYPE_52,
              FDBFILE.FDB_DDECIMAL_52,
              FDBFILE.FDB_DNAME_53,
              FDBFILE.FDB_DOFFSET_53,
              FDBFILE.FDB_DDIGITS_53,
              FDBFILE.FDB_DTYPE_53,
              FDBFILE.FDB_DDECIMAL_53,
              FDBFILE.FDB_DNAME_54,
              FDBFILE.FDB_DOFFSET_54,
              FDBFILE.FDB_DDIGITS_54,
              FDBFILE.FDB_DTYPE_54,
              FDBFILE.FDB_DDECIMAL_54,
              FDBFILE.FDB_DNAME_55,
              FDBFILE.FDB_DOFFSET_55,
              FDBFILE.FDB_DDIGITS_55,
              FDBFILE.FDB_DTYPE_55,
              FDBFILE.FDB_DDECIMAL_55,
              FDBFILE.FDB_DNAME_56,
              FDBFILE.FDB_DOFFSET_56,
              FDBFILE.FDB_DDIGITS_56,
              FDBFILE.FDB_DTYPE_56,
              FDBFILE.FDB_DDECIMAL_56,
              FDBFILE.FDB_DNAME_57,
              FDBFILE.FDB_DOFFSET_57,
              FDBFILE.FDB_DDIGITS_57,
              FDBFILE.FDB_DTYPE_57,
              FDBFILE.FDB_DDECIMAL_57,
              FDBFILE.FDB_DNAME_58,
              FDBFILE.FDB_DOFFSET_58,
              FDBFILE.FDB_DDIGITS_58,
              FDBFILE.FDB_DTYPE_58,
              FDBFILE.FDB_DDECIMAL_58,
              FDBFILE.FDB_DNAME_59,
              FDBFILE.FDB_DOFFSET_59,
              FDBFILE.FDB_DDIGITS_59,
              FDBFILE.FDB_DTYPE_59,
              FDBFILE.FDB_DDECIMAL_59,
              FDBFILE.FDB_DNAME_60,
              FDBFILE.FDB_DOFFSET_60,
              FDBFILE.FDB_DDIGITS_60,
              FDBFILE.FDB_DTYPE_60,
              FDBFILE.FDB_DDECIMAL_60,
              FDBFILE.FDB_DNAME_61,
              FDBFILE.FDB_DOFFSET_61,
              FDBFILE.FDB_DDIGITS_61,
              FDBFILE.FDB_DTYPE_61,
              FDBFILE.FDB_DDECIMAL_61,
              FDBFILE.FDB_DNAME_62,
              FDBFILE.FDB_DOFFSET_62,
              FDBFILE.FDB_DDIGITS_62,
              FDBFILE.FDB_DTYPE_62,
              FDBFILE.FDB_DDECIMAL_62,
              FDBFILE.FDB_DNAME_63,
              FDBFILE.FDB_DOFFSET_63,
              FDBFILE.FDB_DDIGITS_63,
              FDBFILE.FDB_DTYPE_63,
              FDBFILE.FDB_DDECIMAL_63,
              FDBFILE.FDB_DNAME_64,
              FDBFILE.FDB_DOFFSET_64,
              FDBFILE.FDB_DDIGITS_64,
              FDBFILE.FDB_DTYPE_64,
              FDBFILE.FDB_DDECIMAL_64,
              FDBFILE.FDB_DNAME_65,
              FDBFILE.FDB_DOFFSET_65,
              FDBFILE.FDB_DDIGITS_65,
              FDBFILE.FDB_DTYPE_65,
              FDBFILE.FDB_DDECIMAL_65,
              FDBFILE.FDB_DNAME_66,
              FDBFILE.FDB_DOFFSET_66,
              FDBFILE.FDB_DDIGITS_66,
              FDBFILE.FDB_DTYPE_66,
              FDBFILE.FDB_DDECIMAL_66,
              FDBFILE.FDB_DNAME_67,
              FDBFILE.FDB_DOFFSET_67,
              FDBFILE.FDB_DDIGITS_67,
              FDBFILE.FDB_DTYPE_67,
              FDBFILE.FDB_DDECIMAL_67,
              FDBFILE.FDB_DNAME_68,
              FDBFILE.FDB_DOFFSET_68,
              FDBFILE.FDB_DDIGITS_68,
              FDBFILE.FDB_DTYPE_68,
              FDBFILE.FDB_DDECIMAL_68,
              FDBFILE.FDB_DNAME_69,
              FDBFILE.FDB_DOFFSET_69,
              FDBFILE.FDB_DDIGITS_69,
              FDBFILE.FDB_DTYPE_69,
              FDBFILE.FDB_DDECIMAL_69,
              FDBFILE.FDB_DNAME_70,
              FDBFILE.FDB_DOFFSET_70,
              FDBFILE.FDB_DDIGITS_70,
              FDBFILE.FDB_DTYPE_70,
              FDBFILE.FDB_DDECIMAL_70,
              FDBFILE.FDB_DNAME_71,
              FDBFILE.FDB_DOFFSET_71,
              FDBFILE.FDB_DDIGITS_71,
              FDBFILE.FDB_DTYPE_71,
              FDBFILE.FDB_DDECIMAL_71,
              FDBFILE.FDB_DNAME_72,
              FDBFILE.FDB_DOFFSET_72,
              FDBFILE.FDB_DDIGITS_72,
              FDBFILE.FDB_DTYPE_72,
              FDBFILE.FDB_DDECIMAL_72,
              FDBFILE.FDB_DNAME_73,
              FDBFILE.FDB_DOFFSET_73,
              FDBFILE.FDB_DDIGITS_73,
              FDBFILE.FDB_DTYPE_73,
              FDBFILE.FDB_DDECIMAL_73,
              FDBFILE.FDB_DNAME_74,
              FDBFILE.FDB_DOFFSET_74,
              FDBFILE.FDB_DDIGITS_74,
              FDBFILE.FDB_DTYPE_74,
              FDBFILE.FDB_DDECIMAL_74,
              FDBFILE.FDB_DNAME_75,
              FDBFILE.FDB_DOFFSET_75,
              FDBFILE.FDB_DDIGITS_75,
              FDBFILE.FDB_DTYPE_75,
              FDBFILE.FDB_DDECIMAL_75,
              FDBFILE.FDB_DNAME_76,
              FDBFILE.FDB_DOFFSET_76,
              FDBFILE.FDB_DDIGITS_76,
              FDBFILE.FDB_DTYPE_76,
              FDBFILE.FDB_DDECIMAL_76,
              FDBFILE.FDB_DNAME_77,
              FDBFILE.FDB_DOFFSET_77,
              FDBFILE.FDB_DDIGITS_77,
              FDBFILE.FDB_DTYPE_77,
              FDBFILE.FDB_DDECIMAL_77,
              FDBFILE.FDB_DNAME_78,
              FDBFILE.FDB_DOFFSET_78,
              FDBFILE.FDB_DDIGITS_78,
              FDBFILE.FDB_DTYPE_78,
              FDBFILE.FDB_DDECIMAL_78,
              FDBFILE.FDB_DNAME_79,
              FDBFILE.FDB_DOFFSET_79,
              FDBFILE.FDB_DDIGITS_79,
              FDBFILE.FDB_DTYPE_79,
              FDBFILE.FDB_DDECIMAL_79,
              FDBFILE.FDB_DNAME_80,
              FDBFILE.FDB_DOFFSET_80,
              FDBFILE.FDB_DDIGITS_80,
              FDBFILE.FDB_DTYPE_80,
              FDBFILE.FDB_DDECIMAL_80,
              FDBFILE.FDB_DNAME_81,
              FDBFILE.FDB_DOFFSET_81,
              FDBFILE.FDB_DDIGITS_81,
              FDBFILE.FDB_DTYPE_81,
              FDBFILE.FDB_DDECIMAL_81,
              FDBFILE.FDB_DNAME_82,
              FDBFILE.FDB_DOFFSET_82,
              FDBFILE.FDB_DDIGITS_82,
              FDBFILE.FDB_DTYPE_82,
              FDBFILE.FDB_DDECIMAL_82,
              FDBFILE.FDB_DNAME_83,
              FDBFILE.FDB_DOFFSET_83,
              FDBFILE.FDB_DDIGITS_83,
              FDBFILE.FDB_DTYPE_83,
              FDBFILE.FDB_DDECIMAL_83,
              FDBFILE.FDB_DNAME_84,
              FDBFILE.FDB_DOFFSET_84,
              FDBFILE.FDB_DDIGITS_84,
              FDBFILE.FDB_DTYPE_84,
              FDBFILE.FDB_DDECIMAL_84,
              FDBFILE.FDB_DNAME_85,
              FDBFILE.FDB_DOFFSET_85,
              FDBFILE.FDB_DDIGITS_85,
              FDBFILE.FDB_DTYPE_85,
              FDBFILE.FDB_DDECIMAL_85,
              FDBFILE.FDB_DNAME_86,
              FDBFILE.FDB_DOFFSET_86,
              FDBFILE.FDB_DDIGITS_86,
              FDBFILE.FDB_DTYPE_86,
              FDBFILE.FDB_DDECIMAL_86,
              FDBFILE.FDB_DNAME_87,
              FDBFILE.FDB_DOFFSET_87,
              FDBFILE.FDB_DDIGITS_87,
              FDBFILE.FDB_DTYPE_87,
              FDBFILE.FDB_DDECIMAL_87,
              FDBFILE.FDB_DNAME_88,
              FDBFILE.FDB_DOFFSET_88,
              FDBFILE.FDB_DDIGITS_88,
              FDBFILE.FDB_DTYPE_88,
              FDBFILE.FDB_DDECIMAL_88,
              FDBFILE.FDB_DNAME_89,
              FDBFILE.FDB_DOFFSET_89,
              FDBFILE.FDB_DDIGITS_89,
              FDBFILE.FDB_DTYPE_89,
              FDBFILE.FDB_DDECIMAL_89,
              FDBFILE.FDB_DNAME_90,
              FDBFILE.FDB_DOFFSET_90,
              FDBFILE.FDB_DDIGITS_90,
              FDBFILE.FDB_DTYPE_90,
              FDBFILE.FDB_DDECIMAL_90,
              FDBFILE.FDB_DNAME_91,
              FDBFILE.FDB_DOFFSET_91,
              FDBFILE.FDB_DDIGITS_91,
              FDBFILE.FDB_DTYPE_91,
              FDBFILE.FDB_DDECIMAL_91,
              FDBFILE.FDB_DNAME_92,
              FDBFILE.FDB_DOFFSET_92,
              FDBFILE.FDB_DDIGITS_92,
              FDBFILE.FDB_DTYPE_92,
              FDBFILE.FDB_DDECIMAL_92,
              FDBFILE.FDB_DNAME_93,
              FDBFILE.FDB_DOFFSET_93,
              FDBFILE.FDB_DDIGITS_93,
              FDBFILE.FDB_DTYPE_93,
              FDBFILE.FDB_DDECIMAL_93,
              FDBFILE.FDB_DNAME_94,
              FDBFILE.FDB_DOFFSET_94,
              FDBFILE.FDB_DDIGITS_94,
              FDBFILE.FDB_DTYPE_94,
              FDBFILE.FDB_DDECIMAL_94,
              FDBFILE.FDB_DNAME_95,
              FDBFILE.FDB_DOFFSET_95,
              FDBFILE.FDB_DDIGITS_95,
              FDBFILE.FDB_DTYPE_95,
              FDBFILE.FDB_DDECIMAL_95,
              FDBFILE.FDB_DNAME_96,
              FDBFILE.FDB_DOFFSET_96,
              FDBFILE.FDB_DDIGITS_96,
              FDBFILE.FDB_DTYPE_96,
              FDBFILE.FDB_DDECIMAL_96,
              FDBFILE.FDB_DNAME_97,
              FDBFILE.FDB_DOFFSET_97,
              FDBFILE.FDB_DDIGITS_97,
              FDBFILE.FDB_DTYPE_97,
              FDBFILE.FDB_DDECIMAL_97,
              FDBFILE.FDB_DNAME_98,
              FDBFILE.FDB_DOFFSET_98,
              FDBFILE.FDB_DDIGITS_98,
              FDBFILE.FDB_DTYPE_98,
              FDBFILE.FDB_DDECIMAL_98,
              FDBFILE.FDB_DNAME_99,
              FDBFILE.FDB_DOFFSET_99,
              FDBFILE.FDB_DDIGITS_99,
              FDBFILE.FDB_DTYPE_99,
              FDBFILE.FDB_DDECIMAL_99,
              FDBFILE.FDB_DNAME_100,
              FDBFILE.FDB_DOFFSET_100,
              FDBFILE.FDB_DDIGITS_100,
              FDBFILE.FDB_DTYPE_100,
              FDBFILE.FDB_DDECIMAL_100,
              FDBFILE.FDB_DNAME_101,
              FDBFILE.FDB_DOFFSET_101,
              FDBFILE.FDB_DDIGITS_101,
              FDBFILE.FDB_DTYPE_101,
              FDBFILE.FDB_DDECIMAL_101,
              FDBFILE.FDB_DNAME_102,
              FDBFILE.FDB_DOFFSET_102,
              FDBFILE.FDB_DDIGITS_102,
              FDBFILE.FDB_DTYPE_102,
              FDBFILE.FDB_DDECIMAL_102,
              FDBFILE.FDB_DNAME_103,
              FDBFILE.FDB_DOFFSET_103,
              FDBFILE.FDB_DDIGITS_103,
              FDBFILE.FDB_DTYPE_103,
              FDBFILE.FDB_DDECIMAL_103,
              FDBFILE.FDB_DNAME_104,
              FDBFILE.FDB_DOFFSET_104,
              FDBFILE.FDB_DDIGITS_104,
              FDBFILE.FDB_DTYPE_104,
              FDBFILE.FDB_DDECIMAL_104,
              FDBFILE.FDB_DNAME_105,
              FDBFILE.FDB_DOFFSET_105,
              FDBFILE.FDB_DDIGITS_105,
              FDBFILE.FDB_DTYPE_105,
              FDBFILE.FDB_DDECIMAL_105,
              FDBFILE.FDB_DNAME_106,
              FDBFILE.FDB_DOFFSET_106,
              FDBFILE.FDB_DDIGITS_106,
              FDBFILE.FDB_DTYPE_106,
              FDBFILE.FDB_DDECIMAL_106,
              FDBFILE.FDB_DNAME_107,
              FDBFILE.FDB_DOFFSET_107,
              FDBFILE.FDB_DDIGITS_107,
              FDBFILE.FDB_DTYPE_107,
              FDBFILE.FDB_DDECIMAL_107,
              FDBFILE.FDB_DNAME_108,
              FDBFILE.FDB_DOFFSET_108,
              FDBFILE.FDB_DDIGITS_108,
              FDBFILE.FDB_DTYPE_108,
              FDBFILE.FDB_DDECIMAL_108,
              FDBFILE.FDB_DNAME_109,
              FDBFILE.FDB_DOFFSET_109,
              FDBFILE.FDB_DDIGITS_109,
              FDBFILE.FDB_DTYPE_109,
              FDBFILE.FDB_DDECIMAL_109,
              FDBFILE.FDB_DNAME_110,
              FDBFILE.FDB_DOFFSET_110,
              FDBFILE.FDB_DDIGITS_110,
              FDBFILE.FDB_DTYPE_110,
              FDBFILE.FDB_DDECIMAL_110,
              FDBFILE.FDB_DNAME_111,
              FDBFILE.FDB_DOFFSET_111,
              FDBFILE.FDB_DDIGITS_111,
              FDBFILE.FDB_DTYPE_111,
              FDBFILE.FDB_DDECIMAL_111,
              FDBFILE.FDB_DNAME_112,
              FDBFILE.FDB_DOFFSET_112,
              FDBFILE.FDB_DDIGITS_112,
              FDBFILE.FDB_DTYPE_112,
              FDBFILE.FDB_DDECIMAL_112,
              FDBFILE.FDB_DNAME_113,
              FDBFILE.FDB_DOFFSET_113,
              FDBFILE.FDB_DDIGITS_113,
              FDBFILE.FDB_DTYPE_113,
              FDBFILE.FDB_DDECIMAL_113,
              FDBFILE.FDB_DNAME_114,
              FDBFILE.FDB_DOFFSET_114,
              FDBFILE.FDB_DDIGITS_114,
              FDBFILE.FDB_DTYPE_114,
              FDBFILE.FDB_DDECIMAL_114,
              FDBFILE.FDB_DNAME_115,
              FDBFILE.FDB_DOFFSET_115,
              FDBFILE.FDB_DDIGITS_115,
              FDBFILE.FDB_DTYPE_115,
              FDBFILE.FDB_DDECIMAL_115,
              FDBFILE.FDB_DNAME_116,
              FDBFILE.FDB_DOFFSET_116,
              FDBFILE.FDB_DDIGITS_116,
              FDBFILE.FDB_DTYPE_116,
              FDBFILE.FDB_DDECIMAL_116,
              FDBFILE.FDB_DNAME_117,
              FDBFILE.FDB_DOFFSET_117,
              FDBFILE.FDB_DDIGITS_117,
              FDBFILE.FDB_DTYPE_117,
              FDBFILE.FDB_DDECIMAL_117,
              FDBFILE.FDB_DNAME_118,
              FDBFILE.FDB_DOFFSET_118,
              FDBFILE.FDB_DDIGITS_118,
              FDBFILE.FDB_DTYPE_118,
              FDBFILE.FDB_DDECIMAL_118,
              FDBFILE.FDB_DNAME_119,
              FDBFILE.FDB_DOFFSET_119,
              FDBFILE.FDB_DDIGITS_119,
              FDBFILE.FDB_DTYPE_119,
              FDBFILE.FDB_DDECIMAL_119,
              FDBFILE.FDB_DNAME_120,
              FDBFILE.FDB_DOFFSET_120,
              FDBFILE.FDB_DDIGITS_120,
              FDBFILE.FDB_DTYPE_120,
              FDBFILE.FDB_DDECIMAL_120,
              FDBFILE.FDB_DNAME_121,
              FDBFILE.FDB_DOFFSET_121,
              FDBFILE.FDB_DDIGITS_121,
              FDBFILE.FDB_DTYPE_121,
              FDBFILE.FDB_DDECIMAL_121,
              FDBFILE.FDB_DNAME_122,
              FDBFILE.FDB_DOFFSET_122,
              FDBFILE.FDB_DDIGITS_122,
              FDBFILE.FDB_DTYPE_122,
              FDBFILE.FDB_DDECIMAL_122,
              FDBFILE.FDB_DNAME_123,
              FDBFILE.FDB_DOFFSET_123,
              FDBFILE.FDB_DDIGITS_123,
              FDBFILE.FDB_DTYPE_123,
              FDBFILE.FDB_DDECIMAL_123,
              FDBFILE.FDB_DNAME_124,
              FDBFILE.FDB_DOFFSET_124,
              FDBFILE.FDB_DDIGITS_124,
              FDBFILE.FDB_DTYPE_124,
              FDBFILE.FDB_DDECIMAL_124,
              FDBFILE.FDB_DNAME_125,
              FDBFILE.FDB_DOFFSET_125,
              FDBFILE.FDB_DDIGITS_125,
              FDBFILE.FDB_DTYPE_125,
              FDBFILE.FDB_DDECIMAL_125,
              FDBFILE.FDB_DNAME_126,
              FDBFILE.FDB_DOFFSET_126,
              FDBFILE.FDB_DDIGITS_126,
              FDBFILE.FDB_DTYPE_126,
              FDBFILE.FDB_DDECIMAL_126,
              FDBFILE.FDB_DNAME_127,
              FDBFILE.FDB_DOFFSET_127,
              FDBFILE.FDB_DDIGITS_127,
              FDBFILE.FDB_DTYPE_127,
              FDBFILE.FDB_DDECIMAL_127,
              FDBFILE.FDB_DNAME_128,
              FDBFILE.FDB_DOFFSET_128,
              FDBFILE.FDB_DDIGITS_128,
              FDBFILE.FDB_DTYPE_128,
              FDBFILE.FDB_DDECIMAL_128,
              FDBFILE.FDB_DNAME_129,
              FDBFILE.FDB_DOFFSET_129,
              FDBFILE.FDB_DDIGITS_129,
              FDBFILE.FDB_DTYPE_129,
              FDBFILE.FDB_DDECIMAL_129,
              FDBFILE.FDB_DNAME_130,
              FDBFILE.FDB_DOFFSET_130,
              FDBFILE.FDB_DDIGITS_130,
              FDBFILE.FDB_DTYPE_130,
              FDBFILE.FDB_DDECIMAL_130,
              FDBFILE.FDB_DNAME_131,
              FDBFILE.FDB_DOFFSET_131,
              FDBFILE.FDB_DDIGITS_131,
              FDBFILE.FDB_DTYPE_131,
              FDBFILE.FDB_DDECIMAL_131,
              FDBFILE.FDB_DNAME_132,
              FDBFILE.FDB_DOFFSET_132,
              FDBFILE.FDB_DDIGITS_132,
              FDBFILE.FDB_DTYPE_132,
              FDBFILE.FDB_DDECIMAL_132,
              FDBFILE.FDB_DNAME_133,
              FDBFILE.FDB_DOFFSET_133,
              FDBFILE.FDB_DDIGITS_133,
              FDBFILE.FDB_DTYPE_133,
              FDBFILE.FDB_DDECIMAL_133,
              FDBFILE.FDB_DNAME_134,
              FDBFILE.FDB_DOFFSET_134,
              FDBFILE.FDB_DDIGITS_134,
              FDBFILE.FDB_DTYPE_134,
              FDBFILE.FDB_DDECIMAL_134,
              FDBFILE.FDB_DNAME_135,
              FDBFILE.FDB_DOFFSET_135,
              FDBFILE.FDB_DDIGITS_135,
              FDBFILE.FDB_DTYPE_135,
              FDBFILE.FDB_DDECIMAL_135,
              FDBFILE.FDB_DNAME_136,
              FDBFILE.FDB_DOFFSET_136,
              FDBFILE.FDB_DDIGITS_136,
              FDBFILE.FDB_DTYPE_136,
              FDBFILE.FDB_DDECIMAL_136,
              FDBFILE.FDB_DNAME_137,
              FDBFILE.FDB_DOFFSET_137,
              FDBFILE.FDB_DDIGITS_137,
              FDBFILE.FDB_DTYPE_137,
              FDBFILE.FDB_DDECIMAL_137,
              FDBFILE.FDB_DNAME_138,
              FDBFILE.FDB_DOFFSET_138,
              FDBFILE.FDB_DDIGITS_138,
              FDBFILE.FDB_DTYPE_138,
              FDBFILE.FDB_DDECIMAL_138,
              FDBFILE.FDB_DNAME_139,
              FDBFILE.FDB_DOFFSET_139,
              FDBFILE.FDB_DDIGITS_139,
              FDBFILE.FDB_DTYPE_139,
              FDBFILE.FDB_DDECIMAL_139,
              FDBFILE.FDB_DNAME_140,
              FDBFILE.FDB_DOFFSET_140,
              FDBFILE.FDB_DDIGITS_140,
              FDBFILE.FDB_DTYPE_140,
              FDBFILE.FDB_DDECIMAL_140,
              FDBFILE.FDB_DNAME_141,
              FDBFILE.FDB_DOFFSET_141,
              FDBFILE.FDB_DDIGITS_141,
              FDBFILE.FDB_DTYPE_141,
              FDBFILE.FDB_DDECIMAL_141,
              FDBFILE.FDB_DNAME_142,
              FDBFILE.FDB_DOFFSET_142,
              FDBFILE.FDB_DDIGITS_142,
              FDBFILE.FDB_DTYPE_142,
              FDBFILE.FDB_DDECIMAL_142,
              FDBFILE.FDB_DNAME_143,
              FDBFILE.FDB_DOFFSET_143,
              FDBFILE.FDB_DDIGITS_143,
              FDBFILE.FDB_DTYPE_143,
              FDBFILE.FDB_DDECIMAL_143,
              FDBFILE.FDB_DNAME_144,
              FDBFILE.FDB_DOFFSET_144,
              FDBFILE.FDB_DDIGITS_144,
              FDBFILE.FDB_DTYPE_144,
              FDBFILE.FDB_DDECIMAL_144,
              FDBFILE.FDB_DNAME_145,
              FDBFILE.FDB_DOFFSET_145,
              FDBFILE.FDB_DDIGITS_145,
              FDBFILE.FDB_DTYPE_145,
              FDBFILE.FDB_DDECIMAL_145,
              FDBFILE.FDB_DNAME_146,
              FDBFILE.FDB_DOFFSET_146,
              FDBFILE.FDB_DDIGITS_146,
              FDBFILE.FDB_DTYPE_146,
              FDBFILE.FDB_DDECIMAL_146,
              FDBFILE.FDB_DNAME_147,
              FDBFILE.FDB_DOFFSET_147,
              FDBFILE.FDB_DDIGITS_147,
              FDBFILE.FDB_DTYPE_147,
              FDBFILE.FDB_DDECIMAL_147,
              FDBFILE.FDB_DNAME_148,
              FDBFILE.FDB_DOFFSET_148,
              FDBFILE.FDB_DDIGITS_148,
              FDBFILE.FDB_DTYPE_148,
              FDBFILE.FDB_DDECIMAL_148,
              FDBFILE.FDB_DNAME_149,
              FDBFILE.FDB_DOFFSET_149,
              FDBFILE.FDB_DDIGITS_149,
              FDBFILE.FDB_DTYPE_149,
              FDBFILE.FDB_DDECIMAL_149,
              FDBFILE.FDB_DNAME_150,
              FDBFILE.FDB_DOFFSET_150,
              FDBFILE.FDB_DDIGITS_150,
              FDBFILE.FDB_DTYPE_150,
              FDBFILE.FDB_DDECIMAL_150,
              FDBFILE.FDB_DNAME_151,
              FDBFILE.FDB_DOFFSET_151,
              FDBFILE.FDB_DDIGITS_151,
              FDBFILE.FDB_DTYPE_151,
              FDBFILE.FDB_DDECIMAL_151,
              FDBFILE.FDB_DNAME_152,
              FDBFILE.FDB_DOFFSET_152,
              FDBFILE.FDB_DDIGITS_152,
              FDBFILE.FDB_DTYPE_152,
              FDBFILE.FDB_DDECIMAL_152,
              FDBFILE.FDB_DNAME_153,
              FDBFILE.FDB_DOFFSET_153,
              FDBFILE.FDB_DDIGITS_153,
              FDBFILE.FDB_DTYPE_153,
              FDBFILE.FDB_DDECIMAL_153,
              FDBFILE.FDB_DNAME_154,
              FDBFILE.FDB_DOFFSET_154,
              FDBFILE.FDB_DDIGITS_154,
              FDBFILE.FDB_DTYPE_154,
              FDBFILE.FDB_DDECIMAL_154,
              FDBFILE.FDB_DNAME_155,
              FDBFILE.FDB_DOFFSET_155,
              FDBFILE.FDB_DDIGITS_155,
              FDBFILE.FDB_DTYPE_155,
              FDBFILE.FDB_DDECIMAL_155,
              FDBFILE.FDB_DNAME_156,
              FDBFILE.FDB_DOFFSET_156,
              FDBFILE.FDB_DDIGITS_156,
              FDBFILE.FDB_DTYPE_156,
              FDBFILE.FDB_DDECIMAL_156,
              FDBFILE.FDB_DNAME_157,
              FDBFILE.FDB_DOFFSET_157,
              FDBFILE.FDB_DDIGITS_157,
              FDBFILE.FDB_DTYPE_157,
              FDBFILE.FDB_DDECIMAL_157,
              FDBFILE.FDB_DNAME_158,
              FDBFILE.FDB_DOFFSET_158,
              FDBFILE.FDB_DDIGITS_158,
              FDBFILE.FDB_DTYPE_158,
              FDBFILE.FDB_DDECIMAL_158,
              FDBFILE.FDB_DNAME_159,
              FDBFILE.FDB_DOFFSET_159,
              FDBFILE.FDB_DDIGITS_159,
              FDBFILE.FDB_DTYPE_159,
              FDBFILE.FDB_DDECIMAL_159,
              FDBFILE.FDB_DNAME_160,
              FDBFILE.FDB_DOFFSET_160,
              FDBFILE.FDB_DDIGITS_160,
              FDBFILE.FDB_DTYPE_160,
              FDBFILE.FDB_DDECIMAL_160,
              FDBFILE.FDB_DNAME_161,
              FDBFILE.FDB_DOFFSET_161,
              FDBFILE.FDB_DDIGITS_161,
              FDBFILE.FDB_DTYPE_161,
              FDBFILE.FDB_DDECIMAL_161,
              FDBFILE.FDB_DNAME_162,
              FDBFILE.FDB_DOFFSET_162,
              FDBFILE.FDB_DDIGITS_162,
              FDBFILE.FDB_DTYPE_162,
              FDBFILE.FDB_DDECIMAL_162,
              FDBFILE.FDB_DNAME_163,
              FDBFILE.FDB_DOFFSET_163,
              FDBFILE.FDB_DDIGITS_163,
              FDBFILE.FDB_DTYPE_163,
              FDBFILE.FDB_DDECIMAL_163,
              FDBFILE.FDB_DNAME_164,
              FDBFILE.FDB_DOFFSET_164,
              FDBFILE.FDB_DDIGITS_164,
              FDBFILE.FDB_DTYPE_164,
              FDBFILE.FDB_DDECIMAL_164,
              FDBFILE.FDB_DNAME_165,
              FDBFILE.FDB_DOFFSET_165,
              FDBFILE.FDB_DDIGITS_165,
              FDBFILE.FDB_DTYPE_165,
              FDBFILE.FDB_DDECIMAL_165,
              FDBFILE.FDB_DNAME_166,
              FDBFILE.FDB_DOFFSET_166,
              FDBFILE.FDB_DDIGITS_166,
              FDBFILE.FDB_DTYPE_166,
              FDBFILE.FDB_DDECIMAL_166,
              FDBFILE.FDB_DNAME_167,
              FDBFILE.FDB_DOFFSET_167,
              FDBFILE.FDB_DDIGITS_167,
              FDBFILE.FDB_DTYPE_167,
              FDBFILE.FDB_DDECIMAL_167,
              FDBFILE.FDB_DNAME_168,
              FDBFILE.FDB_DOFFSET_168,
              FDBFILE.FDB_DDIGITS_168,
              FDBFILE.FDB_DTYPE_168,
              FDBFILE.FDB_DDECIMAL_168,
              FDBFILE.FDB_DNAME_169,
              FDBFILE.FDB_DOFFSET_169,
              FDBFILE.FDB_DDIGITS_169,
              FDBFILE.FDB_DTYPE_169,
              FDBFILE.FDB_DDECIMAL_169,
              FDBFILE.FDB_DNAME_170,
              FDBFILE.FDB_DOFFSET_170,
              FDBFILE.FDB_DDIGITS_170,
              FDBFILE.FDB_DTYPE_170,
              FDBFILE.FDB_DDECIMAL_170,
              FDBFILE.FDB_DNAME_171,
              FDBFILE.FDB_DOFFSET_171,
              FDBFILE.FDB_DDIGITS_171,
              FDBFILE.FDB_DTYPE_171,
              FDBFILE.FDB_DDECIMAL_171,
              FDBFILE.FDB_DNAME_172,
              FDBFILE.FDB_DOFFSET_172,
              FDBFILE.FDB_DDIGITS_172,
              FDBFILE.FDB_DTYPE_172,
              FDBFILE.FDB_DDECIMAL_172,
              FDBFILE.FDB_DNAME_173,
              FDBFILE.FDB_DOFFSET_173,
              FDBFILE.FDB_DDIGITS_173,
              FDBFILE.FDB_DTYPE_173,
              FDBFILE.FDB_DDECIMAL_173,
              FDBFILE.FDB_DNAME_174,
              FDBFILE.FDB_DOFFSET_174,
              FDBFILE.FDB_DDIGITS_174,
              FDBFILE.FDB_DTYPE_174,
              FDBFILE.FDB_DDECIMAL_174,
              FDBFILE.FDB_DNAME_175,
              FDBFILE.FDB_DOFFSET_175,
              FDBFILE.FDB_DDIGITS_175,
              FDBFILE.FDB_DTYPE_175,
              FDBFILE.FDB_DDECIMAL_175,
              FDBFILE.FDB_DNAME_176,
              FDBFILE.FDB_DOFFSET_176,
              FDBFILE.FDB_DDIGITS_176,
              FDBFILE.FDB_DTYPE_176,
              FDBFILE.FDB_DDECIMAL_176,
              FDBFILE.FDB_DNAME_177,
              FDBFILE.FDB_DOFFSET_177,
              FDBFILE.FDB_DDIGITS_177,
              FDBFILE.FDB_DTYPE_177,
              FDBFILE.FDB_DDECIMAL_177,
              FDBFILE.FDB_DNAME_178,
              FDBFILE.FDB_DOFFSET_178,
              FDBFILE.FDB_DDIGITS_178,
              FDBFILE.FDB_DTYPE_178,
              FDBFILE.FDB_DDECIMAL_178,
              FDBFILE.FDB_DNAME_179,
              FDBFILE.FDB_DOFFSET_179,
              FDBFILE.FDB_DDIGITS_179,
              FDBFILE.FDB_DTYPE_179,
              FDBFILE.FDB_DDECIMAL_179,
              FDBFILE.FDB_DNAME_180,
              FDBFILE.FDB_DOFFSET_180,
              FDBFILE.FDB_DDIGITS_180,
              FDBFILE.FDB_DTYPE_180,
              FDBFILE.FDB_DDECIMAL_180,
              FDBFILE.FDB_DNAME_181,
              FDBFILE.FDB_DOFFSET_181,
              FDBFILE.FDB_DDIGITS_181,
              FDBFILE.FDB_DTYPE_181,
              FDBFILE.FDB_DDECIMAL_181,
              FDBFILE.FDB_DNAME_182,
              FDBFILE.FDB_DOFFSET_182,
              FDBFILE.FDB_DDIGITS_182,
              FDBFILE.FDB_DTYPE_182,
              FDBFILE.FDB_DDECIMAL_182,
              FDBFILE.FDB_DNAME_183,
              FDBFILE.FDB_DOFFSET_183,
              FDBFILE.FDB_DDIGITS_183,
              FDBFILE.FDB_DTYPE_183,
              FDBFILE.FDB_DDECIMAL_183,
              FDBFILE.FDB_DNAME_184,
              FDBFILE.FDB_DOFFSET_184,
              FDBFILE.FDB_DDIGITS_184,
              FDBFILE.FDB_DTYPE_184,
              FDBFILE.FDB_DDECIMAL_184,
              FDBFILE.FDB_DNAME_185,
              FDBFILE.FDB_DOFFSET_185,
              FDBFILE.FDB_DDIGITS_185,
              FDBFILE.FDB_DTYPE_185,
              FDBFILE.FDB_DDECIMAL_185,
              FDBFILE.FDB_DNAME_186,
              FDBFILE.FDB_DOFFSET_186,
              FDBFILE.FDB_DDIGITS_186,
              FDBFILE.FDB_DTYPE_186,
              FDBFILE.FDB_DDECIMAL_186,
              FDBFILE.FDB_DNAME_187,
              FDBFILE.FDB_DOFFSET_187,
              FDBFILE.FDB_DDIGITS_187,
              FDBFILE.FDB_DTYPE_187,
              FDBFILE.FDB_DDECIMAL_187,
              FDBFILE.FDB_DNAME_188,
              FDBFILE.FDB_DOFFSET_188,
              FDBFILE.FDB_DDIGITS_188,
              FDBFILE.FDB_DTYPE_188,
              FDBFILE.FDB_DDECIMAL_188,
              FDBFILE.FDB_DNAME_189,
              FDBFILE.FDB_DOFFSET_189,
              FDBFILE.FDB_DDIGITS_189,
              FDBFILE.FDB_DTYPE_189,
              FDBFILE.FDB_DDECIMAL_189,
              FDBFILE.FDB_DNAME_190,
              FDBFILE.FDB_DOFFSET_190,
              FDBFILE.FDB_DDIGITS_190,
              FDBFILE.FDB_DTYPE_190,
              FDBFILE.FDB_DDECIMAL_190,
              FDBFILE.FDB_DNAME_191,
              FDBFILE.FDB_DOFFSET_191,
              FDBFILE.FDB_DDIGITS_191,
              FDBFILE.FDB_DTYPE_191,
              FDBFILE.FDB_DDECIMAL_191,
              FDBFILE.FDB_DNAME_192,
              FDBFILE.FDB_DOFFSET_192,
              FDBFILE.FDB_DDIGITS_192,
              FDBFILE.FDB_DTYPE_192,
              FDBFILE.FDB_DDECIMAL_192,
              FDBFILE.FDB_DNAME_193,
              FDBFILE.FDB_DOFFSET_193,
              FDBFILE.FDB_DDIGITS_193,
              FDBFILE.FDB_DTYPE_193,
              FDBFILE.FDB_DDECIMAL_193,
              FDBFILE.FDB_DNAME_194,
              FDBFILE.FDB_DOFFSET_194,
              FDBFILE.FDB_DDIGITS_194,
              FDBFILE.FDB_DTYPE_194,
              FDBFILE.FDB_DDECIMAL_194,
              FDBFILE.FDB_DNAME_195,
              FDBFILE.FDB_DOFFSET_195,
              FDBFILE.FDB_DDIGITS_195,
              FDBFILE.FDB_DTYPE_195,
              FDBFILE.FDB_DDECIMAL_195,
              FDBFILE.FDB_DNAME_196,
              FDBFILE.FDB_DOFFSET_196,
              FDBFILE.FDB_DDIGITS_196,
              FDBFILE.FDB_DTYPE_196,
              FDBFILE.FDB_DDECIMAL_196,
              FDBFILE.FDB_DNAME_197,
              FDBFILE.FDB_DOFFSET_197,
              FDBFILE.FDB_DDIGITS_197,
              FDBFILE.FDB_DTYPE_197,
              FDBFILE.FDB_DDECIMAL_197,
              FDBFILE.FDB_DNAME_198,
              FDBFILE.FDB_DOFFSET_198,
              FDBFILE.FDB_DDIGITS_198,
              FDBFILE.FDB_DTYPE_198,
              FDBFILE.FDB_DDECIMAL_198,
              FDBFILE.FDB_DNAME_199,
              FDBFILE.FDB_DOFFSET_199,
              FDBFILE.FDB_DDIGITS_199,
              FDBFILE.FDB_DTYPE_199,
              FDBFILE.FDB_DDECIMAL_199,
              FDBFILE.FDB_DNAME_200,
              FDBFILE.FDB_DOFFSET_200,
              FDBFILE.FDB_DDIGITS_200,
              FDBFILE.FDB_DTYPE_200,
              FDBFILE.FDB_DDECIMAL_200,
              FDBFILE.FDB_CALLID_1,
              FDBFILE.FDB_KEYLOC_1,
              FDBFILE.FDB_CALLID_2,
              FDBFILE.FDB_KEYLOC_2)
            VALUES
             (:QFDB-DBNO,
              NULLIF(:QFDB-LTOUCH-DATE,'1900-01-01'),
              :QFDB-DNAME-1,
              :QFDB-DOFFSET-1,
              :QFDB-DDIGITS-1,
              :QFDB-DTYPE-1,
              :QFDB-DDECIMAL-1,
              :QFDB-DNAME-2,
              :QFDB-DOFFSET-2,
              :QFDB-DDIGITS-2,
              :QFDB-DTYPE-2,
              :QFDB-DDECIMAL-2,
              :QFDB-DNAME-3,
              :QFDB-DOFFSET-3,
              :QFDB-DDIGITS-3,
              :QFDB-DTYPE-3,
              :QFDB-DDECIMAL-3,
              :QFDB-DNAME-4,
              :QFDB-DOFFSET-4,
              :QFDB-DDIGITS-4,
              :QFDB-DTYPE-4,
              :QFDB-DDECIMAL-4,
              :QFDB-DNAME-5,
              :QFDB-DOFFSET-5,
              :QFDB-DDIGITS-5,
              :QFDB-DTYPE-5,
              :QFDB-DDECIMAL-5,
              :QFDB-DNAME-6,
              :QFDB-DOFFSET-6,
              :QFDB-DDIGITS-6,
              :QFDB-DTYPE-6,
              :QFDB-DDECIMAL-6,
              :QFDB-DNAME-7,
              :QFDB-DOFFSET-7,
              :QFDB-DDIGITS-7,
              :QFDB-DTYPE-7,
              :QFDB-DDECIMAL-7,
              :QFDB-DNAME-8,
              :QFDB-DOFFSET-8,
              :QFDB-DDIGITS-8,
              :QFDB-DTYPE-8,
              :QFDB-DDECIMAL-8,
              :QFDB-DNAME-9,
              :QFDB-DOFFSET-9,
              :QFDB-DDIGITS-9,
              :QFDB-DTYPE-9,
              :QFDB-DDECIMAL-9,
              :QFDB-DNAME-10,
              :QFDB-DOFFSET-10,
              :QFDB-DDIGITS-10,
              :QFDB-DTYPE-10,
              :QFDB-DDECIMAL-10,
              :QFDB-DNAME-11,
              :QFDB-DOFFSET-11,
              :QFDB-DDIGITS-11,
              :QFDB-DTYPE-11,
              :QFDB-DDECIMAL-11,
              :QFDB-DNAME-12,
              :QFDB-DOFFSET-12,
              :QFDB-DDIGITS-12,
              :QFDB-DTYPE-12,
              :QFDB-DDECIMAL-12,
              :QFDB-DNAME-13,
              :QFDB-DOFFSET-13,
              :QFDB-DDIGITS-13,
              :QFDB-DTYPE-13,
              :QFDB-DDECIMAL-13,
              :QFDB-DNAME-14,
              :QFDB-DOFFSET-14,
              :QFDB-DDIGITS-14,
              :QFDB-DTYPE-14,
              :QFDB-DDECIMAL-14,
              :QFDB-DNAME-15,
              :QFDB-DOFFSET-15,
              :QFDB-DDIGITS-15,
              :QFDB-DTYPE-15,
              :QFDB-DDECIMAL-15,
              :QFDB-DNAME-16,
              :QFDB-DOFFSET-16,
              :QFDB-DDIGITS-16,
              :QFDB-DTYPE-16,
              :QFDB-DDECIMAL-16,
              :QFDB-DNAME-17,
              :QFDB-DOFFSET-17,
              :QFDB-DDIGITS-17,
              :QFDB-DTYPE-17,
              :QFDB-DDECIMAL-17,
              :QFDB-DNAME-18,
              :QFDB-DOFFSET-18,
              :QFDB-DDIGITS-18,
              :QFDB-DTYPE-18,
              :QFDB-DDECIMAL-18,
              :QFDB-DNAME-19,
              :QFDB-DOFFSET-19,
              :QFDB-DDIGITS-19,
              :QFDB-DTYPE-19,
              :QFDB-DDECIMAL-19,
              :QFDB-DNAME-20,
              :QFDB-DOFFSET-20,
              :QFDB-DDIGITS-20,
              :QFDB-DTYPE-20,
              :QFDB-DDECIMAL-20,
              :QFDB-DNAME-21,
              :QFDB-DOFFSET-21,
              :QFDB-DDIGITS-21,
              :QFDB-DTYPE-21,
              :QFDB-DDECIMAL-21,
              :QFDB-DNAME-22,
              :QFDB-DOFFSET-22,
              :QFDB-DDIGITS-22,
              :QFDB-DTYPE-22,
              :QFDB-DDECIMAL-22,
              :QFDB-DNAME-23,
              :QFDB-DOFFSET-23,
              :QFDB-DDIGITS-23,
              :QFDB-DTYPE-23,
              :QFDB-DDECIMAL-23,
              :QFDB-DNAME-24,
              :QFDB-DOFFSET-24,
              :QFDB-DDIGITS-24,
              :QFDB-DTYPE-24,
              :QFDB-DDECIMAL-24,
              :QFDB-DNAME-25,
              :QFDB-DOFFSET-25,
              :QFDB-DDIGITS-25,
              :QFDB-DTYPE-25,
              :QFDB-DDECIMAL-25,
              :QFDB-DNAME-26,
              :QFDB-DOFFSET-26,
              :QFDB-DDIGITS-26,
              :QFDB-DTYPE-26,
              :QFDB-DDECIMAL-26,
              :QFDB-DNAME-27,
              :QFDB-DOFFSET-27,
              :QFDB-DDIGITS-27,
              :QFDB-DTYPE-27,
              :QFDB-DDECIMAL-27,
              :QFDB-DNAME-28,
              :QFDB-DOFFSET-28,
              :QFDB-DDIGITS-28,
              :QFDB-DTYPE-28,
              :QFDB-DDECIMAL-28,
              :QFDB-DNAME-29,
              :QFDB-DOFFSET-29,
              :QFDB-DDIGITS-29,
              :QFDB-DTYPE-29,
              :QFDB-DDECIMAL-29,
              :QFDB-DNAME-30,
              :QFDB-DOFFSET-30,
              :QFDB-DDIGITS-30,
              :QFDB-DTYPE-30,
              :QFDB-DDECIMAL-30,
              :QFDB-DNAME-31,
              :QFDB-DOFFSET-31,
              :QFDB-DDIGITS-31,
              :QFDB-DTYPE-31,
              :QFDB-DDECIMAL-31,
              :QFDB-DNAME-32,
              :QFDB-DOFFSET-32,
              :QFDB-DDIGITS-32,
              :QFDB-DTYPE-32,
              :QFDB-DDECIMAL-32,
              :QFDB-DNAME-33,
              :QFDB-DOFFSET-33,
              :QFDB-DDIGITS-33,
              :QFDB-DTYPE-33,
              :QFDB-DDECIMAL-33,
              :QFDB-DNAME-34,
              :QFDB-DOFFSET-34,
              :QFDB-DDIGITS-34,
              :QFDB-DTYPE-34,
              :QFDB-DDECIMAL-34,
              :QFDB-DNAME-35,
              :QFDB-DOFFSET-35,
              :QFDB-DDIGITS-35,
              :QFDB-DTYPE-35,
              :QFDB-DDECIMAL-35,
              :QFDB-DNAME-36,
              :QFDB-DOFFSET-36,
              :QFDB-DDIGITS-36,
              :QFDB-DTYPE-36,
              :QFDB-DDECIMAL-36,
              :QFDB-DNAME-37,
              :QFDB-DOFFSET-37,
              :QFDB-DDIGITS-37,
              :QFDB-DTYPE-37,
              :QFDB-DDECIMAL-37,
              :QFDB-DNAME-38,
              :QFDB-DOFFSET-38,
              :QFDB-DDIGITS-38,
              :QFDB-DTYPE-38,
              :QFDB-DDECIMAL-38,
              :QFDB-DNAME-39,
              :QFDB-DOFFSET-39,
              :QFDB-DDIGITS-39,
              :QFDB-DTYPE-39,
              :QFDB-DDECIMAL-39,
              :QFDB-DNAME-40,
              :QFDB-DOFFSET-40,
              :QFDB-DDIGITS-40,
              :QFDB-DTYPE-40,
              :QFDB-DDECIMAL-40,
              :QFDB-DNAME-41,
              :QFDB-DOFFSET-41,
              :QFDB-DDIGITS-41,
              :QFDB-DTYPE-41,
              :QFDB-DDECIMAL-41,
              :QFDB-DNAME-42,
              :QFDB-DOFFSET-42,
              :QFDB-DDIGITS-42,
              :QFDB-DTYPE-42,
              :QFDB-DDECIMAL-42,
              :QFDB-DNAME-43,
              :QFDB-DOFFSET-43,
              :QFDB-DDIGITS-43,
              :QFDB-DTYPE-43,
              :QFDB-DDECIMAL-43,
              :QFDB-DNAME-44,
              :QFDB-DOFFSET-44,
              :QFDB-DDIGITS-44,
              :QFDB-DTYPE-44,
              :QFDB-DDECIMAL-44,
              :QFDB-DNAME-45,
              :QFDB-DOFFSET-45,
              :QFDB-DDIGITS-45,
              :QFDB-DTYPE-45,
              :QFDB-DDECIMAL-45,
              :QFDB-DNAME-46,
              :QFDB-DOFFSET-46,
              :QFDB-DDIGITS-46,
              :QFDB-DTYPE-46,
              :QFDB-DDECIMAL-46,
              :QFDB-DNAME-47,
              :QFDB-DOFFSET-47,
              :QFDB-DDIGITS-47,
              :QFDB-DTYPE-47,
              :QFDB-DDECIMAL-47,
              :QFDB-DNAME-48,
              :QFDB-DOFFSET-48,
              :QFDB-DDIGITS-48,
              :QFDB-DTYPE-48,
              :QFDB-DDECIMAL-48,
              :QFDB-DNAME-49,
              :QFDB-DOFFSET-49,
              :QFDB-DDIGITS-49,
              :QFDB-DTYPE-49,
              :QFDB-DDECIMAL-49,
              :QFDB-DNAME-50,
              :QFDB-DOFFSET-50,
              :QFDB-DDIGITS-50,
              :QFDB-DTYPE-50,
              :QFDB-DDECIMAL-50,
              :QFDB-DNAME-51,
              :QFDB-DOFFSET-51,
              :QFDB-DDIGITS-51,
              :QFDB-DTYPE-51,
              :QFDB-DDECIMAL-51,
              :QFDB-DNAME-52,
              :QFDB-DOFFSET-52,
              :QFDB-DDIGITS-52,
              :QFDB-DTYPE-52,
              :QFDB-DDECIMAL-52,
              :QFDB-DNAME-53,
              :QFDB-DOFFSET-53,
              :QFDB-DDIGITS-53,
              :QFDB-DTYPE-53,
              :QFDB-DDECIMAL-53,
              :QFDB-DNAME-54,
              :QFDB-DOFFSET-54,
              :QFDB-DDIGITS-54,
              :QFDB-DTYPE-54,
              :QFDB-DDECIMAL-54,
              :QFDB-DNAME-55,
              :QFDB-DOFFSET-55,
              :QFDB-DDIGITS-55,
              :QFDB-DTYPE-55,
              :QFDB-DDECIMAL-55,
              :QFDB-DNAME-56,
              :QFDB-DOFFSET-56,
              :QFDB-DDIGITS-56,
              :QFDB-DTYPE-56,
              :QFDB-DDECIMAL-56,
              :QFDB-DNAME-57,
              :QFDB-DOFFSET-57,
              :QFDB-DDIGITS-57,
              :QFDB-DTYPE-57,
              :QFDB-DDECIMAL-57,
              :QFDB-DNAME-58,
              :QFDB-DOFFSET-58,
              :QFDB-DDIGITS-58,
              :QFDB-DTYPE-58,
              :QFDB-DDECIMAL-58,
              :QFDB-DNAME-59,
              :QFDB-DOFFSET-59,
              :QFDB-DDIGITS-59,
              :QFDB-DTYPE-59,
              :QFDB-DDECIMAL-59,
              :QFDB-DNAME-60,
              :QFDB-DOFFSET-60,
              :QFDB-DDIGITS-60,
              :QFDB-DTYPE-60,
              :QFDB-DDECIMAL-60,
              :QFDB-DNAME-61,
              :QFDB-DOFFSET-61,
              :QFDB-DDIGITS-61,
              :QFDB-DTYPE-61,
              :QFDB-DDECIMAL-61,
              :QFDB-DNAME-62,
              :QFDB-DOFFSET-62,
              :QFDB-DDIGITS-62,
              :QFDB-DTYPE-62,
              :QFDB-DDECIMAL-62,
              :QFDB-DNAME-63,
              :QFDB-DOFFSET-63,
              :QFDB-DDIGITS-63,
              :QFDB-DTYPE-63,
              :QFDB-DDECIMAL-63,
              :QFDB-DNAME-64,
              :QFDB-DOFFSET-64,
              :QFDB-DDIGITS-64,
              :QFDB-DTYPE-64,
              :QFDB-DDECIMAL-64,
              :QFDB-DNAME-65,
              :QFDB-DOFFSET-65,
              :QFDB-DDIGITS-65,
              :QFDB-DTYPE-65,
              :QFDB-DDECIMAL-65,
              :QFDB-DNAME-66,
              :QFDB-DOFFSET-66,
              :QFDB-DDIGITS-66,
              :QFDB-DTYPE-66,
              :QFDB-DDECIMAL-66,
              :QFDB-DNAME-67,
              :QFDB-DOFFSET-67,
              :QFDB-DDIGITS-67,
              :QFDB-DTYPE-67,
              :QFDB-DDECIMAL-67,
              :QFDB-DNAME-68,
              :QFDB-DOFFSET-68,
              :QFDB-DDIGITS-68,
              :QFDB-DTYPE-68,
              :QFDB-DDECIMAL-68,
              :QFDB-DNAME-69,
              :QFDB-DOFFSET-69,
              :QFDB-DDIGITS-69,
              :QFDB-DTYPE-69,
              :QFDB-DDECIMAL-69,
              :QFDB-DNAME-70,
              :QFDB-DOFFSET-70,
              :QFDB-DDIGITS-70,
              :QFDB-DTYPE-70,
              :QFDB-DDECIMAL-70,
              :QFDB-DNAME-71,
              :QFDB-DOFFSET-71,
              :QFDB-DDIGITS-71,
              :QFDB-DTYPE-71,
              :QFDB-DDECIMAL-71,
              :QFDB-DNAME-72,
              :QFDB-DOFFSET-72,
              :QFDB-DDIGITS-72,
              :QFDB-DTYPE-72,
              :QFDB-DDECIMAL-72,
              :QFDB-DNAME-73,
              :QFDB-DOFFSET-73,
              :QFDB-DDIGITS-73,
              :QFDB-DTYPE-73,
              :QFDB-DDECIMAL-73,
              :QFDB-DNAME-74,
              :QFDB-DOFFSET-74,
              :QFDB-DDIGITS-74,
              :QFDB-DTYPE-74,
              :QFDB-DDECIMAL-74,
              :QFDB-DNAME-75,
              :QFDB-DOFFSET-75,
              :QFDB-DDIGITS-75,
              :QFDB-DTYPE-75,
              :QFDB-DDECIMAL-75,
              :QFDB-DNAME-76,
              :QFDB-DOFFSET-76,
              :QFDB-DDIGITS-76,
              :QFDB-DTYPE-76,
              :QFDB-DDECIMAL-76,
              :QFDB-DNAME-77,
              :QFDB-DOFFSET-77,
              :QFDB-DDIGITS-77,
              :QFDB-DTYPE-77,
              :QFDB-DDECIMAL-77,
              :QFDB-DNAME-78,
              :QFDB-DOFFSET-78,
              :QFDB-DDIGITS-78,
              :QFDB-DTYPE-78,
              :QFDB-DDECIMAL-78,
              :QFDB-DNAME-79,
              :QFDB-DOFFSET-79,
              :QFDB-DDIGITS-79,
              :QFDB-DTYPE-79,
              :QFDB-DDECIMAL-79,
              :QFDB-DNAME-80,
              :QFDB-DOFFSET-80,
              :QFDB-DDIGITS-80,
              :QFDB-DTYPE-80,
              :QFDB-DDECIMAL-80,
              :QFDB-DNAME-81,
              :QFDB-DOFFSET-81,
              :QFDB-DDIGITS-81,
              :QFDB-DTYPE-81,
              :QFDB-DDECIMAL-81,
              :QFDB-DNAME-82,
              :QFDB-DOFFSET-82,
              :QFDB-DDIGITS-82,
              :QFDB-DTYPE-82,
              :QFDB-DDECIMAL-82,
              :QFDB-DNAME-83,
              :QFDB-DOFFSET-83,
              :QFDB-DDIGITS-83,
              :QFDB-DTYPE-83,
              :QFDB-DDECIMAL-83,
              :QFDB-DNAME-84,
              :QFDB-DOFFSET-84,
              :QFDB-DDIGITS-84,
              :QFDB-DTYPE-84,
              :QFDB-DDECIMAL-84,
              :QFDB-DNAME-85,
              :QFDB-DOFFSET-85,
              :QFDB-DDIGITS-85,
              :QFDB-DTYPE-85,
              :QFDB-DDECIMAL-85,
              :QFDB-DNAME-86,
              :QFDB-DOFFSET-86,
              :QFDB-DDIGITS-86,
              :QFDB-DTYPE-86,
              :QFDB-DDECIMAL-86,
              :QFDB-DNAME-87,
              :QFDB-DOFFSET-87,
              :QFDB-DDIGITS-87,
              :QFDB-DTYPE-87,
              :QFDB-DDECIMAL-87,
              :QFDB-DNAME-88,
              :QFDB-DOFFSET-88,
              :QFDB-DDIGITS-88,
              :QFDB-DTYPE-88,
              :QFDB-DDECIMAL-88,
              :QFDB-DNAME-89,
              :QFDB-DOFFSET-89,
              :QFDB-DDIGITS-89,
              :QFDB-DTYPE-89,
              :QFDB-DDECIMAL-89,
              :QFDB-DNAME-90,
              :QFDB-DOFFSET-90,
              :QFDB-DDIGITS-90,
              :QFDB-DTYPE-90,
              :QFDB-DDECIMAL-90,
              :QFDB-DNAME-91,
              :QFDB-DOFFSET-91,
              :QFDB-DDIGITS-91,
              :QFDB-DTYPE-91,
              :QFDB-DDECIMAL-91,
              :QFDB-DNAME-92,
              :QFDB-DOFFSET-92,
              :QFDB-DDIGITS-92,
              :QFDB-DTYPE-92,
              :QFDB-DDECIMAL-92,
              :QFDB-DNAME-93,
              :QFDB-DOFFSET-93,
              :QFDB-DDIGITS-93,
              :QFDB-DTYPE-93,
              :QFDB-DDECIMAL-93,
              :QFDB-DNAME-94,
              :QFDB-DOFFSET-94,
              :QFDB-DDIGITS-94,
              :QFDB-DTYPE-94,
              :QFDB-DDECIMAL-94,
              :QFDB-DNAME-95,
              :QFDB-DOFFSET-95,
              :QFDB-DDIGITS-95,
              :QFDB-DTYPE-95,
              :QFDB-DDECIMAL-95,
              :QFDB-DNAME-96,
              :QFDB-DOFFSET-96,
              :QFDB-DDIGITS-96,
              :QFDB-DTYPE-96,
              :QFDB-DDECIMAL-96,
              :QFDB-DNAME-97,
              :QFDB-DOFFSET-97,
              :QFDB-DDIGITS-97,
              :QFDB-DTYPE-97,
              :QFDB-DDECIMAL-97,
              :QFDB-DNAME-98,
              :QFDB-DOFFSET-98,
              :QFDB-DDIGITS-98,
              :QFDB-DTYPE-98,
              :QFDB-DDECIMAL-98,
              :QFDB-DNAME-99,
              :QFDB-DOFFSET-99,
              :QFDB-DDIGITS-99,
              :QFDB-DTYPE-99,
              :QFDB-DDECIMAL-99,
              :QFDB-DNAME-100,
              :QFDB-DOFFSET-100,
              :QFDB-DDIGITS-100,
              :QFDB-DTYPE-100,
              :QFDB-DDECIMAL-100,
              :QFDB-DNAME-101,
              :QFDB-DOFFSET-101,
              :QFDB-DDIGITS-101,
              :QFDB-DTYPE-101,
              :QFDB-DDECIMAL-101,
              :QFDB-DNAME-102,
              :QFDB-DOFFSET-102,
              :QFDB-DDIGITS-102,
              :QFDB-DTYPE-102,
              :QFDB-DDECIMAL-102,
              :QFDB-DNAME-103,
              :QFDB-DOFFSET-103,
              :QFDB-DDIGITS-103,
              :QFDB-DTYPE-103,
              :QFDB-DDECIMAL-103,
              :QFDB-DNAME-104,
              :QFDB-DOFFSET-104,
              :QFDB-DDIGITS-104,
              :QFDB-DTYPE-104,
              :QFDB-DDECIMAL-104,
              :QFDB-DNAME-105,
              :QFDB-DOFFSET-105,
              :QFDB-DDIGITS-105,
              :QFDB-DTYPE-105,
              :QFDB-DDECIMAL-105,
              :QFDB-DNAME-106,
              :QFDB-DOFFSET-106,
              :QFDB-DDIGITS-106,
              :QFDB-DTYPE-106,
              :QFDB-DDECIMAL-106,
              :QFDB-DNAME-107,
              :QFDB-DOFFSET-107,
              :QFDB-DDIGITS-107,
              :QFDB-DTYPE-107,
              :QFDB-DDECIMAL-107,
              :QFDB-DNAME-108,
              :QFDB-DOFFSET-108,
              :QFDB-DDIGITS-108,
              :QFDB-DTYPE-108,
              :QFDB-DDECIMAL-108,
              :QFDB-DNAME-109,
              :QFDB-DOFFSET-109,
              :QFDB-DDIGITS-109,
              :QFDB-DTYPE-109,
              :QFDB-DDECIMAL-109,
              :QFDB-DNAME-110,
              :QFDB-DOFFSET-110,
              :QFDB-DDIGITS-110,
              :QFDB-DTYPE-110,
              :QFDB-DDECIMAL-110,
              :QFDB-DNAME-111,
              :QFDB-DOFFSET-111,
              :QFDB-DDIGITS-111,
              :QFDB-DTYPE-111,
              :QFDB-DDECIMAL-111,
              :QFDB-DNAME-112,
              :QFDB-DOFFSET-112,
              :QFDB-DDIGITS-112,
              :QFDB-DTYPE-112,
              :QFDB-DDECIMAL-112,
              :QFDB-DNAME-113,
              :QFDB-DOFFSET-113,
              :QFDB-DDIGITS-113,
              :QFDB-DTYPE-113,
              :QFDB-DDECIMAL-113,
              :QFDB-DNAME-114,
              :QFDB-DOFFSET-114,
              :QFDB-DDIGITS-114,
              :QFDB-DTYPE-114,
              :QFDB-DDECIMAL-114,
              :QFDB-DNAME-115,
              :QFDB-DOFFSET-115,
              :QFDB-DDIGITS-115,
              :QFDB-DTYPE-115,
              :QFDB-DDECIMAL-115,
              :QFDB-DNAME-116,
              :QFDB-DOFFSET-116,
              :QFDB-DDIGITS-116,
              :QFDB-DTYPE-116,
              :QFDB-DDECIMAL-116,
              :QFDB-DNAME-117,
              :QFDB-DOFFSET-117,
              :QFDB-DDIGITS-117,
              :QFDB-DTYPE-117,
              :QFDB-DDECIMAL-117,
              :QFDB-DNAME-118,
              :QFDB-DOFFSET-118,
              :QFDB-DDIGITS-118,
              :QFDB-DTYPE-118,
              :QFDB-DDECIMAL-118,
              :QFDB-DNAME-119,
              :QFDB-DOFFSET-119,
              :QFDB-DDIGITS-119,
              :QFDB-DTYPE-119,
              :QFDB-DDECIMAL-119,
              :QFDB-DNAME-120,
              :QFDB-DOFFSET-120,
              :QFDB-DDIGITS-120,
              :QFDB-DTYPE-120,
              :QFDB-DDECIMAL-120,
              :QFDB-DNAME-121,
              :QFDB-DOFFSET-121,
              :QFDB-DDIGITS-121,
              :QFDB-DTYPE-121,
              :QFDB-DDECIMAL-121,
              :QFDB-DNAME-122,
              :QFDB-DOFFSET-122,
              :QFDB-DDIGITS-122,
              :QFDB-DTYPE-122,
              :QFDB-DDECIMAL-122,
              :QFDB-DNAME-123,
              :QFDB-DOFFSET-123,
              :QFDB-DDIGITS-123,
              :QFDB-DTYPE-123,
              :QFDB-DDECIMAL-123,
              :QFDB-DNAME-124,
              :QFDB-DOFFSET-124,
              :QFDB-DDIGITS-124,
              :QFDB-DTYPE-124,
              :QFDB-DDECIMAL-124,
              :QFDB-DNAME-125,
              :QFDB-DOFFSET-125,
              :QFDB-DDIGITS-125,
              :QFDB-DTYPE-125,
              :QFDB-DDECIMAL-125,
              :QFDB-DNAME-126,
              :QFDB-DOFFSET-126,
              :QFDB-DDIGITS-126,
              :QFDB-DTYPE-126,
              :QFDB-DDECIMAL-126,
              :QFDB-DNAME-127,
              :QFDB-DOFFSET-127,
              :QFDB-DDIGITS-127,
              :QFDB-DTYPE-127,
              :QFDB-DDECIMAL-127,
              :QFDB-DNAME-128,
              :QFDB-DOFFSET-128,
              :QFDB-DDIGITS-128,
              :QFDB-DTYPE-128,
              :QFDB-DDECIMAL-128,
              :QFDB-DNAME-129,
              :QFDB-DOFFSET-129,
              :QFDB-DDIGITS-129,
              :QFDB-DTYPE-129,
              :QFDB-DDECIMAL-129,
              :QFDB-DNAME-130,
              :QFDB-DOFFSET-130,
              :QFDB-DDIGITS-130,
              :QFDB-DTYPE-130,
              :QFDB-DDECIMAL-130,
              :QFDB-DNAME-131,
              :QFDB-DOFFSET-131,
              :QFDB-DDIGITS-131,
              :QFDB-DTYPE-131,
              :QFDB-DDECIMAL-131,
              :QFDB-DNAME-132,
              :QFDB-DOFFSET-132,
              :QFDB-DDIGITS-132,
              :QFDB-DTYPE-132,
              :QFDB-DDECIMAL-132,
              :QFDB-DNAME-133,
              :QFDB-DOFFSET-133,
              :QFDB-DDIGITS-133,
              :QFDB-DTYPE-133,
              :QFDB-DDECIMAL-133,
              :QFDB-DNAME-134,
              :QFDB-DOFFSET-134,
              :QFDB-DDIGITS-134,
              :QFDB-DTYPE-134,
              :QFDB-DDECIMAL-134,
              :QFDB-DNAME-135,
              :QFDB-DOFFSET-135,
              :QFDB-DDIGITS-135,
              :QFDB-DTYPE-135,
              :QFDB-DDECIMAL-135,
              :QFDB-DNAME-136,
              :QFDB-DOFFSET-136,
              :QFDB-DDIGITS-136,
              :QFDB-DTYPE-136,
              :QFDB-DDECIMAL-136,
              :QFDB-DNAME-137,
              :QFDB-DOFFSET-137,
              :QFDB-DDIGITS-137,
              :QFDB-DTYPE-137,
              :QFDB-DDECIMAL-137,
              :QFDB-DNAME-138,
              :QFDB-DOFFSET-138,
              :QFDB-DDIGITS-138,
              :QFDB-DTYPE-138,
              :QFDB-DDECIMAL-138,
              :QFDB-DNAME-139,
              :QFDB-DOFFSET-139,
              :QFDB-DDIGITS-139,
              :QFDB-DTYPE-139,
              :QFDB-DDECIMAL-139,
              :QFDB-DNAME-140,
              :QFDB-DOFFSET-140,
              :QFDB-DDIGITS-140,
              :QFDB-DTYPE-140,
              :QFDB-DDECIMAL-140,
              :QFDB-DNAME-141,
              :QFDB-DOFFSET-141,
              :QFDB-DDIGITS-141,
              :QFDB-DTYPE-141,
              :QFDB-DDECIMAL-141,
              :QFDB-DNAME-142,
              :QFDB-DOFFSET-142,
              :QFDB-DDIGITS-142,
              :QFDB-DTYPE-142,
              :QFDB-DDECIMAL-142,
              :QFDB-DNAME-143,
              :QFDB-DOFFSET-143,
              :QFDB-DDIGITS-143,
              :QFDB-DTYPE-143,
              :QFDB-DDECIMAL-143,
              :QFDB-DNAME-144,
              :QFDB-DOFFSET-144,
              :QFDB-DDIGITS-144,
              :QFDB-DTYPE-144,
              :QFDB-DDECIMAL-144,
              :QFDB-DNAME-145,
              :QFDB-DOFFSET-145,
              :QFDB-DDIGITS-145,
              :QFDB-DTYPE-145,
              :QFDB-DDECIMAL-145,
              :QFDB-DNAME-146,
              :QFDB-DOFFSET-146,
              :QFDB-DDIGITS-146,
              :QFDB-DTYPE-146,
              :QFDB-DDECIMAL-146,
              :QFDB-DNAME-147,
              :QFDB-DOFFSET-147,
              :QFDB-DDIGITS-147,
              :QFDB-DTYPE-147,
              :QFDB-DDECIMAL-147,
              :QFDB-DNAME-148,
              :QFDB-DOFFSET-148,
              :QFDB-DDIGITS-148,
              :QFDB-DTYPE-148,
              :QFDB-DDECIMAL-148,
              :QFDB-DNAME-149,
              :QFDB-DOFFSET-149,
              :QFDB-DDIGITS-149,
              :QFDB-DTYPE-149,
              :QFDB-DDECIMAL-149,
              :QFDB-DNAME-150,
              :QFDB-DOFFSET-150,
              :QFDB-DDIGITS-150,
              :QFDB-DTYPE-150,
              :QFDB-DDECIMAL-150,
              :QFDB-DNAME-151,
              :QFDB-DOFFSET-151,
              :QFDB-DDIGITS-151,
              :QFDB-DTYPE-151,
              :QFDB-DDECIMAL-151,
              :QFDB-DNAME-152,
              :QFDB-DOFFSET-152,
              :QFDB-DDIGITS-152,
              :QFDB-DTYPE-152,
              :QFDB-DDECIMAL-152,
              :QFDB-DNAME-153,
              :QFDB-DOFFSET-153,
              :QFDB-DDIGITS-153,
              :QFDB-DTYPE-153,
              :QFDB-DDECIMAL-153,
              :QFDB-DNAME-154,
              :QFDB-DOFFSET-154,
              :QFDB-DDIGITS-154,
              :QFDB-DTYPE-154,
              :QFDB-DDECIMAL-154,
              :QFDB-DNAME-155,
              :QFDB-DOFFSET-155,
              :QFDB-DDIGITS-155,
              :QFDB-DTYPE-155,
              :QFDB-DDECIMAL-155,
              :QFDB-DNAME-156,
              :QFDB-DOFFSET-156,
              :QFDB-DDIGITS-156,
              :QFDB-DTYPE-156,
              :QFDB-DDECIMAL-156,
              :QFDB-DNAME-157,
              :QFDB-DOFFSET-157,
              :QFDB-DDIGITS-157,
              :QFDB-DTYPE-157,
              :QFDB-DDECIMAL-157,
              :QFDB-DNAME-158,
              :QFDB-DOFFSET-158,
              :QFDB-DDIGITS-158,
              :QFDB-DTYPE-158,
              :QFDB-DDECIMAL-158,
              :QFDB-DNAME-159,
              :QFDB-DOFFSET-159,
              :QFDB-DDIGITS-159,
              :QFDB-DTYPE-159,
              :QFDB-DDECIMAL-159,
              :QFDB-DNAME-160,
              :QFDB-DOFFSET-160,
              :QFDB-DDIGITS-160,
              :QFDB-DTYPE-160,
              :QFDB-DDECIMAL-160,
              :QFDB-DNAME-161,
              :QFDB-DOFFSET-161,
              :QFDB-DDIGITS-161,
              :QFDB-DTYPE-161,
              :QFDB-DDECIMAL-161,
              :QFDB-DNAME-162,
              :QFDB-DOFFSET-162,
              :QFDB-DDIGITS-162,
              :QFDB-DTYPE-162,
              :QFDB-DDECIMAL-162,
              :QFDB-DNAME-163,
              :QFDB-DOFFSET-163,
              :QFDB-DDIGITS-163,
              :QFDB-DTYPE-163,
              :QFDB-DDECIMAL-163,
              :QFDB-DNAME-164,
              :QFDB-DOFFSET-164,
              :QFDB-DDIGITS-164,
              :QFDB-DTYPE-164,
              :QFDB-DDECIMAL-164,
              :QFDB-DNAME-165,
              :QFDB-DOFFSET-165,
              :QFDB-DDIGITS-165,
              :QFDB-DTYPE-165,
              :QFDB-DDECIMAL-165,
              :QFDB-DNAME-166,
              :QFDB-DOFFSET-166,
              :QFDB-DDIGITS-166,
              :QFDB-DTYPE-166,
              :QFDB-DDECIMAL-166,
              :QFDB-DNAME-167,
              :QFDB-DOFFSET-167,
              :QFDB-DDIGITS-167,
              :QFDB-DTYPE-167,
              :QFDB-DDECIMAL-167,
              :QFDB-DNAME-168,
              :QFDB-DOFFSET-168,
              :QFDB-DDIGITS-168,
              :QFDB-DTYPE-168,
              :QFDB-DDECIMAL-168,
              :QFDB-DNAME-169,
              :QFDB-DOFFSET-169,
              :QFDB-DDIGITS-169,
              :QFDB-DTYPE-169,
              :QFDB-DDECIMAL-169,
              :QFDB-DNAME-170,
              :QFDB-DOFFSET-170,
              :QFDB-DDIGITS-170,
              :QFDB-DTYPE-170,
              :QFDB-DDECIMAL-170,
              :QFDB-DNAME-171,
              :QFDB-DOFFSET-171,
              :QFDB-DDIGITS-171,
              :QFDB-DTYPE-171,
              :QFDB-DDECIMAL-171,
              :QFDB-DNAME-172,
              :QFDB-DOFFSET-172,
              :QFDB-DDIGITS-172,
              :QFDB-DTYPE-172,
              :QFDB-DDECIMAL-172,
              :QFDB-DNAME-173,
              :QFDB-DOFFSET-173,
              :QFDB-DDIGITS-173,
              :QFDB-DTYPE-173,
              :QFDB-DDECIMAL-173,
              :QFDB-DNAME-174,
              :QFDB-DOFFSET-174,
              :QFDB-DDIGITS-174,
              :QFDB-DTYPE-174,
              :QFDB-DDECIMAL-174,
              :QFDB-DNAME-175,
              :QFDB-DOFFSET-175,
              :QFDB-DDIGITS-175,
              :QFDB-DTYPE-175,
              :QFDB-DDECIMAL-175,
              :QFDB-DNAME-176,
              :QFDB-DOFFSET-176,
              :QFDB-DDIGITS-176,
              :QFDB-DTYPE-176,
              :QFDB-DDECIMAL-176,
              :QFDB-DNAME-177,
              :QFDB-DOFFSET-177,
              :QFDB-DDIGITS-177,
              :QFDB-DTYPE-177,
              :QFDB-DDECIMAL-177,
              :QFDB-DNAME-178,
              :QFDB-DOFFSET-178,
              :QFDB-DDIGITS-178,
              :QFDB-DTYPE-178,
              :QFDB-DDECIMAL-178,
              :QFDB-DNAME-179,
              :QFDB-DOFFSET-179,
              :QFDB-DDIGITS-179,
              :QFDB-DTYPE-179,
              :QFDB-DDECIMAL-179,
              :QFDB-DNAME-180,
              :QFDB-DOFFSET-180,
              :QFDB-DDIGITS-180,
              :QFDB-DTYPE-180,
              :QFDB-DDECIMAL-180,
              :QFDB-DNAME-181,
              :QFDB-DOFFSET-181,
              :QFDB-DDIGITS-181,
              :QFDB-DTYPE-181,
              :QFDB-DDECIMAL-181,
              :QFDB-DNAME-182,
              :QFDB-DOFFSET-182,
              :QFDB-DDIGITS-182,
              :QFDB-DTYPE-182,
              :QFDB-DDECIMAL-182,
              :QFDB-DNAME-183,
              :QFDB-DOFFSET-183,
              :QFDB-DDIGITS-183,
              :QFDB-DTYPE-183,
              :QFDB-DDECIMAL-183,
              :QFDB-DNAME-184,
              :QFDB-DOFFSET-184,
              :QFDB-DDIGITS-184,
              :QFDB-DTYPE-184,
              :QFDB-DDECIMAL-184,
              :QFDB-DNAME-185,
              :QFDB-DOFFSET-185,
              :QFDB-DDIGITS-185,
              :QFDB-DTYPE-185,
              :QFDB-DDECIMAL-185,
              :QFDB-DNAME-186,
              :QFDB-DOFFSET-186,
              :QFDB-DDIGITS-186,
              :QFDB-DTYPE-186,
              :QFDB-DDECIMAL-186,
              :QFDB-DNAME-187,
              :QFDB-DOFFSET-187,
              :QFDB-DDIGITS-187,
              :QFDB-DTYPE-187,
              :QFDB-DDECIMAL-187,
              :QFDB-DNAME-188,
              :QFDB-DOFFSET-188,
              :QFDB-DDIGITS-188,
              :QFDB-DTYPE-188,
              :QFDB-DDECIMAL-188,
              :QFDB-DNAME-189,
              :QFDB-DOFFSET-189,
              :QFDB-DDIGITS-189,
              :QFDB-DTYPE-189,
              :QFDB-DDECIMAL-189,
              :QFDB-DNAME-190,
              :QFDB-DOFFSET-190,
              :QFDB-DDIGITS-190,
              :QFDB-DTYPE-190,
              :QFDB-DDECIMAL-190,
              :QFDB-DNAME-191,
              :QFDB-DOFFSET-191,
              :QFDB-DDIGITS-191,
              :QFDB-DTYPE-191,
              :QFDB-DDECIMAL-191,
              :QFDB-DNAME-192,
              :QFDB-DOFFSET-192,
              :QFDB-DDIGITS-192,
              :QFDB-DTYPE-192,
              :QFDB-DDECIMAL-192,
              :QFDB-DNAME-193,
              :QFDB-DOFFSET-193,
              :QFDB-DDIGITS-193,
              :QFDB-DTYPE-193,
              :QFDB-DDECIMAL-193,
              :QFDB-DNAME-194,
              :QFDB-DOFFSET-194,
              :QFDB-DDIGITS-194,
              :QFDB-DTYPE-194,
              :QFDB-DDECIMAL-194,
              :QFDB-DNAME-195,
              :QFDB-DOFFSET-195,
              :QFDB-DDIGITS-195,
              :QFDB-DTYPE-195,
              :QFDB-DDECIMAL-195,
              :QFDB-DNAME-196,
              :QFDB-DOFFSET-196,
              :QFDB-DDIGITS-196,
              :QFDB-DTYPE-196,
              :QFDB-DDECIMAL-196,
              :QFDB-DNAME-197,
              :QFDB-DOFFSET-197,
              :QFDB-DDIGITS-197,
              :QFDB-DTYPE-197,
              :QFDB-DDECIMAL-197,
              :QFDB-DNAME-198,
              :QFDB-DOFFSET-198,
              :QFDB-DDIGITS-198,
              :QFDB-DTYPE-198,
              :QFDB-DDECIMAL-198,
              :QFDB-DNAME-199,
              :QFDB-DOFFSET-199,
              :QFDB-DDIGITS-199,
              :QFDB-DTYPE-199,
              :QFDB-DDECIMAL-199,
              :QFDB-DNAME-200,
              :QFDB-DOFFSET-200,
              :QFDB-DDIGITS-200,
              :QFDB-DTYPE-200,
              :QFDB-DDECIMAL-200,
              :QFDB-CALLID-1,
              :QFDB-KEYLOC-1,
              :QFDB-CALLID-2,
              :QFDB-KEYLOC-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-FDB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-FDB1-FILE.

           ACCEPT FDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.
           PERFORM SET-FDB-FIELDS.

           EXEC SQL
            UPDATE DBO.FDBFILE
             SET
              FDBFILE.FDB_DBNO = :QFDB-DBNO,
              FDBFILE.FDB_LTOUCH_DATE =
                 NULLIF(:QFDB-LTOUCH-DATE,'1900-01-01'),
              FDBFILE.FDB_DNAME_1 = :QFDB-DNAME-1,
              FDBFILE.FDB_DOFFSET_1 = :QFDB-DOFFSET-1,
              FDBFILE.FDB_DDIGITS_1 = :QFDB-DDIGITS-1,
              FDBFILE.FDB_DTYPE_1 = :QFDB-DTYPE-1,
              FDBFILE.FDB_DDECIMAL_1 = :QFDB-DDECIMAL-1,
              FDBFILE.FDB_DNAME_2 = :QFDB-DNAME-2,
              FDBFILE.FDB_DOFFSET_2 = :QFDB-DOFFSET-2,
              FDBFILE.FDB_DDIGITS_2 = :QFDB-DDIGITS-2,
              FDBFILE.FDB_DTYPE_2 = :QFDB-DTYPE-2,
              FDBFILE.FDB_DDECIMAL_2 = :QFDB-DDECIMAL-2,
              FDBFILE.FDB_DNAME_3 = :QFDB-DNAME-3,
              FDBFILE.FDB_DOFFSET_3 = :QFDB-DOFFSET-3,
              FDBFILE.FDB_DDIGITS_3 = :QFDB-DDIGITS-3,
              FDBFILE.FDB_DTYPE_3 = :QFDB-DTYPE-3,
              FDBFILE.FDB_DDECIMAL_3 = :QFDB-DDECIMAL-3,
              FDBFILE.FDB_DNAME_4 = :QFDB-DNAME-4,
              FDBFILE.FDB_DOFFSET_4 = :QFDB-DOFFSET-4,
              FDBFILE.FDB_DDIGITS_4 = :QFDB-DDIGITS-4,
              FDBFILE.FDB_DTYPE_4 = :QFDB-DTYPE-4,
              FDBFILE.FDB_DDECIMAL_4 = :QFDB-DDECIMAL-4,
              FDBFILE.FDB_DNAME_5 = :QFDB-DNAME-5,
              FDBFILE.FDB_DOFFSET_5 = :QFDB-DOFFSET-5,
              FDBFILE.FDB_DDIGITS_5 = :QFDB-DDIGITS-5,
              FDBFILE.FDB_DTYPE_5 = :QFDB-DTYPE-5,
              FDBFILE.FDB_DDECIMAL_5 = :QFDB-DDECIMAL-5,
              FDBFILE.FDB_DNAME_6 = :QFDB-DNAME-6,
              FDBFILE.FDB_DOFFSET_6 = :QFDB-DOFFSET-6,
              FDBFILE.FDB_DDIGITS_6 = :QFDB-DDIGITS-6,
              FDBFILE.FDB_DTYPE_6 = :QFDB-DTYPE-6,
              FDBFILE.FDB_DDECIMAL_6 = :QFDB-DDECIMAL-6,
              FDBFILE.FDB_DNAME_7 = :QFDB-DNAME-7,
              FDBFILE.FDB_DOFFSET_7 = :QFDB-DOFFSET-7,
              FDBFILE.FDB_DDIGITS_7 = :QFDB-DDIGITS-7,
              FDBFILE.FDB_DTYPE_7 = :QFDB-DTYPE-7,
              FDBFILE.FDB_DDECIMAL_7 = :QFDB-DDECIMAL-7,
              FDBFILE.FDB_DNAME_8 = :QFDB-DNAME-8,
              FDBFILE.FDB_DOFFSET_8 = :QFDB-DOFFSET-8,
              FDBFILE.FDB_DDIGITS_8 = :QFDB-DDIGITS-8,
              FDBFILE.FDB_DTYPE_8 = :QFDB-DTYPE-8,
              FDBFILE.FDB_DDECIMAL_8 = :QFDB-DDECIMAL-8,
              FDBFILE.FDB_DNAME_9 = :QFDB-DNAME-9,
              FDBFILE.FDB_DOFFSET_9 = :QFDB-DOFFSET-9,
              FDBFILE.FDB_DDIGITS_9 = :QFDB-DDIGITS-9,
              FDBFILE.FDB_DTYPE_9 = :QFDB-DTYPE-9,
              FDBFILE.FDB_DDECIMAL_9 = :QFDB-DDECIMAL-9,
              FDBFILE.FDB_DNAME_10 = :QFDB-DNAME-10,
              FDBFILE.FDB_DOFFSET_10 = :QFDB-DOFFSET-10,
              FDBFILE.FDB_DDIGITS_10 = :QFDB-DDIGITS-10,
              FDBFILE.FDB_DTYPE_10 = :QFDB-DTYPE-10,
              FDBFILE.FDB_DDECIMAL_10 = :QFDB-DDECIMAL-10,
              FDBFILE.FDB_DNAME_11 = :QFDB-DNAME-11,
              FDBFILE.FDB_DOFFSET_11 = :QFDB-DOFFSET-11,
              FDBFILE.FDB_DDIGITS_11 = :QFDB-DDIGITS-11,
              FDBFILE.FDB_DTYPE_11 = :QFDB-DTYPE-11,
              FDBFILE.FDB_DDECIMAL_11 = :QFDB-DDECIMAL-11,
              FDBFILE.FDB_DNAME_12 = :QFDB-DNAME-12,
              FDBFILE.FDB_DOFFSET_12 = :QFDB-DOFFSET-12,
              FDBFILE.FDB_DDIGITS_12 = :QFDB-DDIGITS-12,
              FDBFILE.FDB_DTYPE_12 = :QFDB-DTYPE-12,
              FDBFILE.FDB_DDECIMAL_12 = :QFDB-DDECIMAL-12,
              FDBFILE.FDB_DNAME_13 = :QFDB-DNAME-13,
              FDBFILE.FDB_DOFFSET_13 = :QFDB-DOFFSET-13,
              FDBFILE.FDB_DDIGITS_13 = :QFDB-DDIGITS-13,
              FDBFILE.FDB_DTYPE_13 = :QFDB-DTYPE-13,
              FDBFILE.FDB_DDECIMAL_13 = :QFDB-DDECIMAL-13,
              FDBFILE.FDB_DNAME_14 = :QFDB-DNAME-14,
              FDBFILE.FDB_DOFFSET_14 = :QFDB-DOFFSET-14,
              FDBFILE.FDB_DDIGITS_14 = :QFDB-DDIGITS-14,
              FDBFILE.FDB_DTYPE_14 = :QFDB-DTYPE-14,
              FDBFILE.FDB_DDECIMAL_14 = :QFDB-DDECIMAL-14,
              FDBFILE.FDB_DNAME_15 = :QFDB-DNAME-15,
              FDBFILE.FDB_DOFFSET_15 = :QFDB-DOFFSET-15,
              FDBFILE.FDB_DDIGITS_15 = :QFDB-DDIGITS-15,
              FDBFILE.FDB_DTYPE_15 = :QFDB-DTYPE-15,
              FDBFILE.FDB_DDECIMAL_15 = :QFDB-DDECIMAL-15,
              FDBFILE.FDB_DNAME_16 = :QFDB-DNAME-16,
              FDBFILE.FDB_DOFFSET_16 = :QFDB-DOFFSET-16,
              FDBFILE.FDB_DDIGITS_16 = :QFDB-DDIGITS-16,
              FDBFILE.FDB_DTYPE_16 = :QFDB-DTYPE-16,
              FDBFILE.FDB_DDECIMAL_16 = :QFDB-DDECIMAL-16,
              FDBFILE.FDB_DNAME_17 = :QFDB-DNAME-17,
              FDBFILE.FDB_DOFFSET_17 = :QFDB-DOFFSET-17,
              FDBFILE.FDB_DDIGITS_17 = :QFDB-DDIGITS-17,
              FDBFILE.FDB_DTYPE_17 = :QFDB-DTYPE-17,
              FDBFILE.FDB_DDECIMAL_17 = :QFDB-DDECIMAL-17,
              FDBFILE.FDB_DNAME_18 = :QFDB-DNAME-18,
              FDBFILE.FDB_DOFFSET_18 = :QFDB-DOFFSET-18,
              FDBFILE.FDB_DDIGITS_18 = :QFDB-DDIGITS-18,
              FDBFILE.FDB_DTYPE_18 = :QFDB-DTYPE-18,
              FDBFILE.FDB_DDECIMAL_18 = :QFDB-DDECIMAL-18,
              FDBFILE.FDB_DNAME_19 = :QFDB-DNAME-19,
              FDBFILE.FDB_DOFFSET_19 = :QFDB-DOFFSET-19,
              FDBFILE.FDB_DDIGITS_19 = :QFDB-DDIGITS-19,
              FDBFILE.FDB_DTYPE_19 = :QFDB-DTYPE-19,
              FDBFILE.FDB_DDECIMAL_19 = :QFDB-DDECIMAL-19,
              FDBFILE.FDB_DNAME_20 = :QFDB-DNAME-20,
              FDBFILE.FDB_DOFFSET_20 = :QFDB-DOFFSET-20,
              FDBFILE.FDB_DDIGITS_20 = :QFDB-DDIGITS-20,
              FDBFILE.FDB_DTYPE_20 = :QFDB-DTYPE-20,
              FDBFILE.FDB_DDECIMAL_20 = :QFDB-DDECIMAL-20,
              FDBFILE.FDB_DNAME_21 = :QFDB-DNAME-21,
              FDBFILE.FDB_DOFFSET_21 = :QFDB-DOFFSET-21,
              FDBFILE.FDB_DDIGITS_21 = :QFDB-DDIGITS-21,
              FDBFILE.FDB_DTYPE_21 = :QFDB-DTYPE-21,
              FDBFILE.FDB_DDECIMAL_21 = :QFDB-DDECIMAL-21,
              FDBFILE.FDB_DNAME_22 = :QFDB-DNAME-22,
              FDBFILE.FDB_DOFFSET_22 = :QFDB-DOFFSET-22,
              FDBFILE.FDB_DDIGITS_22 = :QFDB-DDIGITS-22,
              FDBFILE.FDB_DTYPE_22 = :QFDB-DTYPE-22,
              FDBFILE.FDB_DDECIMAL_22 = :QFDB-DDECIMAL-22,
              FDBFILE.FDB_DNAME_23 = :QFDB-DNAME-23,
              FDBFILE.FDB_DOFFSET_23 = :QFDB-DOFFSET-23,
              FDBFILE.FDB_DDIGITS_23 = :QFDB-DDIGITS-23,
              FDBFILE.FDB_DTYPE_23 = :QFDB-DTYPE-23,
              FDBFILE.FDB_DDECIMAL_23 = :QFDB-DDECIMAL-23,
              FDBFILE.FDB_DNAME_24 = :QFDB-DNAME-24,
              FDBFILE.FDB_DOFFSET_24 = :QFDB-DOFFSET-24,
              FDBFILE.FDB_DDIGITS_24 = :QFDB-DDIGITS-24,
              FDBFILE.FDB_DTYPE_24 = :QFDB-DTYPE-24,
              FDBFILE.FDB_DDECIMAL_24 = :QFDB-DDECIMAL-24,
              FDBFILE.FDB_DNAME_25 = :QFDB-DNAME-25,
              FDBFILE.FDB_DOFFSET_25 = :QFDB-DOFFSET-25,
              FDBFILE.FDB_DDIGITS_25 = :QFDB-DDIGITS-25,
              FDBFILE.FDB_DTYPE_25 = :QFDB-DTYPE-25,
              FDBFILE.FDB_DDECIMAL_25 = :QFDB-DDECIMAL-25,
              FDBFILE.FDB_DNAME_26 = :QFDB-DNAME-26,
              FDBFILE.FDB_DOFFSET_26 = :QFDB-DOFFSET-26,
              FDBFILE.FDB_DDIGITS_26 = :QFDB-DDIGITS-26,
              FDBFILE.FDB_DTYPE_26 = :QFDB-DTYPE-26,
              FDBFILE.FDB_DDECIMAL_26 = :QFDB-DDECIMAL-26,
              FDBFILE.FDB_DNAME_27 = :QFDB-DNAME-27,
              FDBFILE.FDB_DOFFSET_27 = :QFDB-DOFFSET-27,
              FDBFILE.FDB_DDIGITS_27 = :QFDB-DDIGITS-27,
              FDBFILE.FDB_DTYPE_27 = :QFDB-DTYPE-27,
              FDBFILE.FDB_DDECIMAL_27 = :QFDB-DDECIMAL-27,
              FDBFILE.FDB_DNAME_28 = :QFDB-DNAME-28,
              FDBFILE.FDB_DOFFSET_28 = :QFDB-DOFFSET-28,
              FDBFILE.FDB_DDIGITS_28 = :QFDB-DDIGITS-28,
              FDBFILE.FDB_DTYPE_28 = :QFDB-DTYPE-28,
              FDBFILE.FDB_DDECIMAL_28 = :QFDB-DDECIMAL-28,
              FDBFILE.FDB_DNAME_29 = :QFDB-DNAME-29,
              FDBFILE.FDB_DOFFSET_29 = :QFDB-DOFFSET-29,
              FDBFILE.FDB_DDIGITS_29 = :QFDB-DDIGITS-29,
              FDBFILE.FDB_DTYPE_29 = :QFDB-DTYPE-29,
              FDBFILE.FDB_DDECIMAL_29 = :QFDB-DDECIMAL-29,
              FDBFILE.FDB_DNAME_30 = :QFDB-DNAME-30,
              FDBFILE.FDB_DOFFSET_30 = :QFDB-DOFFSET-30,
              FDBFILE.FDB_DDIGITS_30 = :QFDB-DDIGITS-30,
              FDBFILE.FDB_DTYPE_30 = :QFDB-DTYPE-30,
              FDBFILE.FDB_DDECIMAL_30 = :QFDB-DDECIMAL-30,
              FDBFILE.FDB_DNAME_31 = :QFDB-DNAME-31,
              FDBFILE.FDB_DOFFSET_31 = :QFDB-DOFFSET-31,
              FDBFILE.FDB_DDIGITS_31 = :QFDB-DDIGITS-31,
              FDBFILE.FDB_DTYPE_31 = :QFDB-DTYPE-31,
              FDBFILE.FDB_DDECIMAL_31 = :QFDB-DDECIMAL-31,
              FDBFILE.FDB_DNAME_32 = :QFDB-DNAME-32,
              FDBFILE.FDB_DOFFSET_32 = :QFDB-DOFFSET-32,
              FDBFILE.FDB_DDIGITS_32 = :QFDB-DDIGITS-32,
              FDBFILE.FDB_DTYPE_32 = :QFDB-DTYPE-32,
              FDBFILE.FDB_DDECIMAL_32 = :QFDB-DDECIMAL-32,
              FDBFILE.FDB_DNAME_33 = :QFDB-DNAME-33,
              FDBFILE.FDB_DOFFSET_33 = :QFDB-DOFFSET-33,
              FDBFILE.FDB_DDIGITS_33 = :QFDB-DDIGITS-33,
              FDBFILE.FDB_DTYPE_33 = :QFDB-DTYPE-33,
              FDBFILE.FDB_DDECIMAL_33 = :QFDB-DDECIMAL-33,
              FDBFILE.FDB_DNAME_34 = :QFDB-DNAME-34,
              FDBFILE.FDB_DOFFSET_34 = :QFDB-DOFFSET-34,
              FDBFILE.FDB_DDIGITS_34 = :QFDB-DDIGITS-34,
              FDBFILE.FDB_DTYPE_34 = :QFDB-DTYPE-34,
              FDBFILE.FDB_DDECIMAL_34 = :QFDB-DDECIMAL-34,
              FDBFILE.FDB_DNAME_35 = :QFDB-DNAME-35,
              FDBFILE.FDB_DOFFSET_35 = :QFDB-DOFFSET-35,
              FDBFILE.FDB_DDIGITS_35 = :QFDB-DDIGITS-35,
              FDBFILE.FDB_DTYPE_35 = :QFDB-DTYPE-35,
              FDBFILE.FDB_DDECIMAL_35 = :QFDB-DDECIMAL-35,
              FDBFILE.FDB_DNAME_36 = :QFDB-DNAME-36,
              FDBFILE.FDB_DOFFSET_36 = :QFDB-DOFFSET-36,
              FDBFILE.FDB_DDIGITS_36 = :QFDB-DDIGITS-36,
              FDBFILE.FDB_DTYPE_36 = :QFDB-DTYPE-36,
              FDBFILE.FDB_DDECIMAL_36 = :QFDB-DDECIMAL-36,
              FDBFILE.FDB_DNAME_37 = :QFDB-DNAME-37,
              FDBFILE.FDB_DOFFSET_37 = :QFDB-DOFFSET-37,
              FDBFILE.FDB_DDIGITS_37 = :QFDB-DDIGITS-37,
              FDBFILE.FDB_DTYPE_37 = :QFDB-DTYPE-37,
              FDBFILE.FDB_DDECIMAL_37 = :QFDB-DDECIMAL-37,
              FDBFILE.FDB_DNAME_38 = :QFDB-DNAME-38,
              FDBFILE.FDB_DOFFSET_38 = :QFDB-DOFFSET-38,
              FDBFILE.FDB_DDIGITS_38 = :QFDB-DDIGITS-38,
              FDBFILE.FDB_DTYPE_38 = :QFDB-DTYPE-38,
              FDBFILE.FDB_DDECIMAL_38 = :QFDB-DDECIMAL-38,
              FDBFILE.FDB_DNAME_39 = :QFDB-DNAME-39,
              FDBFILE.FDB_DOFFSET_39 = :QFDB-DOFFSET-39,
              FDBFILE.FDB_DDIGITS_39 = :QFDB-DDIGITS-39,
              FDBFILE.FDB_DTYPE_39 = :QFDB-DTYPE-39,
              FDBFILE.FDB_DDECIMAL_39 = :QFDB-DDECIMAL-39,
              FDBFILE.FDB_DNAME_40 = :QFDB-DNAME-40,
              FDBFILE.FDB_DOFFSET_40 = :QFDB-DOFFSET-40,
              FDBFILE.FDB_DDIGITS_40 = :QFDB-DDIGITS-40,
              FDBFILE.FDB_DTYPE_40 = :QFDB-DTYPE-40,
              FDBFILE.FDB_DDECIMAL_40 = :QFDB-DDECIMAL-40,
              FDBFILE.FDB_DNAME_41 = :QFDB-DNAME-41,
              FDBFILE.FDB_DOFFSET_41 = :QFDB-DOFFSET-41,
              FDBFILE.FDB_DDIGITS_41 = :QFDB-DDIGITS-41,
              FDBFILE.FDB_DTYPE_41 = :QFDB-DTYPE-41,
              FDBFILE.FDB_DDECIMAL_41 = :QFDB-DDECIMAL-41,
              FDBFILE.FDB_DNAME_42 = :QFDB-DNAME-42,
              FDBFILE.FDB_DOFFSET_42 = :QFDB-DOFFSET-42,
              FDBFILE.FDB_DDIGITS_42 = :QFDB-DDIGITS-42,
              FDBFILE.FDB_DTYPE_42 = :QFDB-DTYPE-42,
              FDBFILE.FDB_DDECIMAL_42 = :QFDB-DDECIMAL-42,
              FDBFILE.FDB_DNAME_43 = :QFDB-DNAME-43,
              FDBFILE.FDB_DOFFSET_43 = :QFDB-DOFFSET-43,
              FDBFILE.FDB_DDIGITS_43 = :QFDB-DDIGITS-43,
              FDBFILE.FDB_DTYPE_43 = :QFDB-DTYPE-43,
              FDBFILE.FDB_DDECIMAL_43 = :QFDB-DDECIMAL-43,
              FDBFILE.FDB_DNAME_44 = :QFDB-DNAME-44,
              FDBFILE.FDB_DOFFSET_44 = :QFDB-DOFFSET-44,
              FDBFILE.FDB_DDIGITS_44 = :QFDB-DDIGITS-44,
              FDBFILE.FDB_DTYPE_44 = :QFDB-DTYPE-44,
              FDBFILE.FDB_DDECIMAL_44 = :QFDB-DDECIMAL-44,
              FDBFILE.FDB_DNAME_45 = :QFDB-DNAME-45,
              FDBFILE.FDB_DOFFSET_45 = :QFDB-DOFFSET-45,
              FDBFILE.FDB_DDIGITS_45 = :QFDB-DDIGITS-45,
              FDBFILE.FDB_DTYPE_45 = :QFDB-DTYPE-45,
              FDBFILE.FDB_DDECIMAL_45 = :QFDB-DDECIMAL-45,
              FDBFILE.FDB_DNAME_46 = :QFDB-DNAME-46,
              FDBFILE.FDB_DOFFSET_46 = :QFDB-DOFFSET-46,
              FDBFILE.FDB_DDIGITS_46 = :QFDB-DDIGITS-46,
              FDBFILE.FDB_DTYPE_46 = :QFDB-DTYPE-46,
              FDBFILE.FDB_DDECIMAL_46 = :QFDB-DDECIMAL-46,
              FDBFILE.FDB_DNAME_47 = :QFDB-DNAME-47,
              FDBFILE.FDB_DOFFSET_47 = :QFDB-DOFFSET-47,
              FDBFILE.FDB_DDIGITS_47 = :QFDB-DDIGITS-47,
              FDBFILE.FDB_DTYPE_47 = :QFDB-DTYPE-47,
              FDBFILE.FDB_DDECIMAL_47 = :QFDB-DDECIMAL-47,
              FDBFILE.FDB_DNAME_48 = :QFDB-DNAME-48,
              FDBFILE.FDB_DOFFSET_48 = :QFDB-DOFFSET-48,
              FDBFILE.FDB_DDIGITS_48 = :QFDB-DDIGITS-48,
              FDBFILE.FDB_DTYPE_48 = :QFDB-DTYPE-48,
              FDBFILE.FDB_DDECIMAL_48 = :QFDB-DDECIMAL-48,
              FDBFILE.FDB_DNAME_49 = :QFDB-DNAME-49,
              FDBFILE.FDB_DOFFSET_49 = :QFDB-DOFFSET-49,
              FDBFILE.FDB_DDIGITS_49 = :QFDB-DDIGITS-49,
              FDBFILE.FDB_DTYPE_49 = :QFDB-DTYPE-49,
              FDBFILE.FDB_DDECIMAL_49 = :QFDB-DDECIMAL-49,
              FDBFILE.FDB_DNAME_50 = :QFDB-DNAME-50,
              FDBFILE.FDB_DOFFSET_50 = :QFDB-DOFFSET-50,
              FDBFILE.FDB_DDIGITS_50 = :QFDB-DDIGITS-50,
              FDBFILE.FDB_DTYPE_50 = :QFDB-DTYPE-50,
              FDBFILE.FDB_DDECIMAL_50 = :QFDB-DDECIMAL-50,
              FDBFILE.FDB_DNAME_51 = :QFDB-DNAME-51,
              FDBFILE.FDB_DOFFSET_51 = :QFDB-DOFFSET-51,
              FDBFILE.FDB_DDIGITS_51 = :QFDB-DDIGITS-51,
              FDBFILE.FDB_DTYPE_51 = :QFDB-DTYPE-51,
              FDBFILE.FDB_DDECIMAL_51 = :QFDB-DDECIMAL-51,
              FDBFILE.FDB_DNAME_52 = :QFDB-DNAME-52,
              FDBFILE.FDB_DOFFSET_52 = :QFDB-DOFFSET-52,
              FDBFILE.FDB_DDIGITS_52 = :QFDB-DDIGITS-52,
              FDBFILE.FDB_DTYPE_52 = :QFDB-DTYPE-52,
              FDBFILE.FDB_DDECIMAL_52 = :QFDB-DDECIMAL-52,
              FDBFILE.FDB_DNAME_53 = :QFDB-DNAME-53,
              FDBFILE.FDB_DOFFSET_53 = :QFDB-DOFFSET-53,
              FDBFILE.FDB_DDIGITS_53 = :QFDB-DDIGITS-53,
              FDBFILE.FDB_DTYPE_53 = :QFDB-DTYPE-53,
              FDBFILE.FDB_DDECIMAL_53 = :QFDB-DDECIMAL-53,
              FDBFILE.FDB_DNAME_54 = :QFDB-DNAME-54,
              FDBFILE.FDB_DOFFSET_54 = :QFDB-DOFFSET-54,
              FDBFILE.FDB_DDIGITS_54 = :QFDB-DDIGITS-54,
              FDBFILE.FDB_DTYPE_54 = :QFDB-DTYPE-54,
              FDBFILE.FDB_DDECIMAL_54 = :QFDB-DDECIMAL-54,
              FDBFILE.FDB_DNAME_55 = :QFDB-DNAME-55,
              FDBFILE.FDB_DOFFSET_55 = :QFDB-DOFFSET-55,
              FDBFILE.FDB_DDIGITS_55 = :QFDB-DDIGITS-55,
              FDBFILE.FDB_DTYPE_55 = :QFDB-DTYPE-55,
              FDBFILE.FDB_DDECIMAL_55 = :QFDB-DDECIMAL-55,
              FDBFILE.FDB_DNAME_56 = :QFDB-DNAME-56,
              FDBFILE.FDB_DOFFSET_56 = :QFDB-DOFFSET-56,
              FDBFILE.FDB_DDIGITS_56 = :QFDB-DDIGITS-56,
              FDBFILE.FDB_DTYPE_56 = :QFDB-DTYPE-56,
              FDBFILE.FDB_DDECIMAL_56 = :QFDB-DDECIMAL-56,
              FDBFILE.FDB_DNAME_57 = :QFDB-DNAME-57,
              FDBFILE.FDB_DOFFSET_57 = :QFDB-DOFFSET-57,
              FDBFILE.FDB_DDIGITS_57 = :QFDB-DDIGITS-57,
              FDBFILE.FDB_DTYPE_57 = :QFDB-DTYPE-57,
              FDBFILE.FDB_DDECIMAL_57 = :QFDB-DDECIMAL-57,
              FDBFILE.FDB_DNAME_58 = :QFDB-DNAME-58,
              FDBFILE.FDB_DOFFSET_58 = :QFDB-DOFFSET-58,
              FDBFILE.FDB_DDIGITS_58 = :QFDB-DDIGITS-58,
              FDBFILE.FDB_DTYPE_58 = :QFDB-DTYPE-58,
              FDBFILE.FDB_DDECIMAL_58 = :QFDB-DDECIMAL-58,
              FDBFILE.FDB_DNAME_59 = :QFDB-DNAME-59,
              FDBFILE.FDB_DOFFSET_59 = :QFDB-DOFFSET-59,
              FDBFILE.FDB_DDIGITS_59 = :QFDB-DDIGITS-59,
              FDBFILE.FDB_DTYPE_59 = :QFDB-DTYPE-59,
              FDBFILE.FDB_DDECIMAL_59 = :QFDB-DDECIMAL-59,
              FDBFILE.FDB_DNAME_60 = :QFDB-DNAME-60,
              FDBFILE.FDB_DOFFSET_60 = :QFDB-DOFFSET-60,
              FDBFILE.FDB_DDIGITS_60 = :QFDB-DDIGITS-60,
              FDBFILE.FDB_DTYPE_60 = :QFDB-DTYPE-60,
              FDBFILE.FDB_DDECIMAL_60 = :QFDB-DDECIMAL-60,
              FDBFILE.FDB_DNAME_61 = :QFDB-DNAME-61,
              FDBFILE.FDB_DOFFSET_61 = :QFDB-DOFFSET-61,
              FDBFILE.FDB_DDIGITS_61 = :QFDB-DDIGITS-61,
              FDBFILE.FDB_DTYPE_61 = :QFDB-DTYPE-61,
              FDBFILE.FDB_DDECIMAL_61 = :QFDB-DDECIMAL-61,
              FDBFILE.FDB_DNAME_62 = :QFDB-DNAME-62,
              FDBFILE.FDB_DOFFSET_62 = :QFDB-DOFFSET-62,
              FDBFILE.FDB_DDIGITS_62 = :QFDB-DDIGITS-62,
              FDBFILE.FDB_DTYPE_62 = :QFDB-DTYPE-62,
              FDBFILE.FDB_DDECIMAL_62 = :QFDB-DDECIMAL-62,
              FDBFILE.FDB_DNAME_63 = :QFDB-DNAME-63,
              FDBFILE.FDB_DOFFSET_63 = :QFDB-DOFFSET-63,
              FDBFILE.FDB_DDIGITS_63 = :QFDB-DDIGITS-63,
              FDBFILE.FDB_DTYPE_63 = :QFDB-DTYPE-63,
              FDBFILE.FDB_DDECIMAL_63 = :QFDB-DDECIMAL-63,
              FDBFILE.FDB_DNAME_64 = :QFDB-DNAME-64,
              FDBFILE.FDB_DOFFSET_64 = :QFDB-DOFFSET-64,
              FDBFILE.FDB_DDIGITS_64 = :QFDB-DDIGITS-64,
              FDBFILE.FDB_DTYPE_64 = :QFDB-DTYPE-64,
              FDBFILE.FDB_DDECIMAL_64 = :QFDB-DDECIMAL-64,
              FDBFILE.FDB_DNAME_65 = :QFDB-DNAME-65,
              FDBFILE.FDB_DOFFSET_65 = :QFDB-DOFFSET-65,
              FDBFILE.FDB_DDIGITS_65 = :QFDB-DDIGITS-65,
              FDBFILE.FDB_DTYPE_65 = :QFDB-DTYPE-65,
              FDBFILE.FDB_DDECIMAL_65 = :QFDB-DDECIMAL-65,
              FDBFILE.FDB_DNAME_66 = :QFDB-DNAME-66,
              FDBFILE.FDB_DOFFSET_66 = :QFDB-DOFFSET-66,
              FDBFILE.FDB_DDIGITS_66 = :QFDB-DDIGITS-66,
              FDBFILE.FDB_DTYPE_66 = :QFDB-DTYPE-66,
              FDBFILE.FDB_DDECIMAL_66 = :QFDB-DDECIMAL-66,
              FDBFILE.FDB_DNAME_67 = :QFDB-DNAME-67,
              FDBFILE.FDB_DOFFSET_67 = :QFDB-DOFFSET-67,
              FDBFILE.FDB_DDIGITS_67 = :QFDB-DDIGITS-67,
              FDBFILE.FDB_DTYPE_67 = :QFDB-DTYPE-67,
              FDBFILE.FDB_DDECIMAL_67 = :QFDB-DDECIMAL-67,
              FDBFILE.FDB_DNAME_68 = :QFDB-DNAME-68,
              FDBFILE.FDB_DOFFSET_68 = :QFDB-DOFFSET-68,
              FDBFILE.FDB_DDIGITS_68 = :QFDB-DDIGITS-68,
              FDBFILE.FDB_DTYPE_68 = :QFDB-DTYPE-68,
              FDBFILE.FDB_DDECIMAL_68 = :QFDB-DDECIMAL-68,
              FDBFILE.FDB_DNAME_69 = :QFDB-DNAME-69,
              FDBFILE.FDB_DOFFSET_69 = :QFDB-DOFFSET-69,
              FDBFILE.FDB_DDIGITS_69 = :QFDB-DDIGITS-69,
              FDBFILE.FDB_DTYPE_69 = :QFDB-DTYPE-69,
              FDBFILE.FDB_DDECIMAL_69 = :QFDB-DDECIMAL-69,
              FDBFILE.FDB_DNAME_70 = :QFDB-DNAME-70,
              FDBFILE.FDB_DOFFSET_70 = :QFDB-DOFFSET-70,
              FDBFILE.FDB_DDIGITS_70 = :QFDB-DDIGITS-70,
              FDBFILE.FDB_DTYPE_70 = :QFDB-DTYPE-70,
              FDBFILE.FDB_DDECIMAL_70 = :QFDB-DDECIMAL-70,
              FDBFILE.FDB_DNAME_71 = :QFDB-DNAME-71,
              FDBFILE.FDB_DOFFSET_71 = :QFDB-DOFFSET-71,
              FDBFILE.FDB_DDIGITS_71 = :QFDB-DDIGITS-71,
              FDBFILE.FDB_DTYPE_71 = :QFDB-DTYPE-71,
              FDBFILE.FDB_DDECIMAL_71 = :QFDB-DDECIMAL-71,
              FDBFILE.FDB_DNAME_72 = :QFDB-DNAME-72,
              FDBFILE.FDB_DOFFSET_72 = :QFDB-DOFFSET-72,
              FDBFILE.FDB_DDIGITS_72 = :QFDB-DDIGITS-72,
              FDBFILE.FDB_DTYPE_72 = :QFDB-DTYPE-72,
              FDBFILE.FDB_DDECIMAL_72 = :QFDB-DDECIMAL-72,
              FDBFILE.FDB_DNAME_73 = :QFDB-DNAME-73,
              FDBFILE.FDB_DOFFSET_73 = :QFDB-DOFFSET-73,
              FDBFILE.FDB_DDIGITS_73 = :QFDB-DDIGITS-73,
              FDBFILE.FDB_DTYPE_73 = :QFDB-DTYPE-73,
              FDBFILE.FDB_DDECIMAL_73 = :QFDB-DDECIMAL-73,
              FDBFILE.FDB_DNAME_74 = :QFDB-DNAME-74,
              FDBFILE.FDB_DOFFSET_74 = :QFDB-DOFFSET-74,
              FDBFILE.FDB_DDIGITS_74 = :QFDB-DDIGITS-74,
              FDBFILE.FDB_DTYPE_74 = :QFDB-DTYPE-74,
              FDBFILE.FDB_DDECIMAL_74 = :QFDB-DDECIMAL-74,
              FDBFILE.FDB_DNAME_75 = :QFDB-DNAME-75,
              FDBFILE.FDB_DOFFSET_75 = :QFDB-DOFFSET-75,
              FDBFILE.FDB_DDIGITS_75 = :QFDB-DDIGITS-75,
              FDBFILE.FDB_DTYPE_75 = :QFDB-DTYPE-75,
              FDBFILE.FDB_DDECIMAL_75 = :QFDB-DDECIMAL-75,
              FDBFILE.FDB_DNAME_76 = :QFDB-DNAME-76,
              FDBFILE.FDB_DOFFSET_76 = :QFDB-DOFFSET-76,
              FDBFILE.FDB_DDIGITS_76 = :QFDB-DDIGITS-76,
              FDBFILE.FDB_DTYPE_76 = :QFDB-DTYPE-76,
              FDBFILE.FDB_DDECIMAL_76 = :QFDB-DDECIMAL-76,
              FDBFILE.FDB_DNAME_77 = :QFDB-DNAME-77,
              FDBFILE.FDB_DOFFSET_77 = :QFDB-DOFFSET-77,
              FDBFILE.FDB_DDIGITS_77 = :QFDB-DDIGITS-77,
              FDBFILE.FDB_DTYPE_77 = :QFDB-DTYPE-77,
              FDBFILE.FDB_DDECIMAL_77 = :QFDB-DDECIMAL-77,
              FDBFILE.FDB_DNAME_78 = :QFDB-DNAME-78,
              FDBFILE.FDB_DOFFSET_78 = :QFDB-DOFFSET-78,
              FDBFILE.FDB_DDIGITS_78 = :QFDB-DDIGITS-78,
              FDBFILE.FDB_DTYPE_78 = :QFDB-DTYPE-78,
              FDBFILE.FDB_DDECIMAL_78 = :QFDB-DDECIMAL-78,
              FDBFILE.FDB_DNAME_79 = :QFDB-DNAME-79,
              FDBFILE.FDB_DOFFSET_79 = :QFDB-DOFFSET-79,
              FDBFILE.FDB_DDIGITS_79 = :QFDB-DDIGITS-79,
              FDBFILE.FDB_DTYPE_79 = :QFDB-DTYPE-79,
              FDBFILE.FDB_DDECIMAL_79 = :QFDB-DDECIMAL-79,
              FDBFILE.FDB_DNAME_80 = :QFDB-DNAME-80,
              FDBFILE.FDB_DOFFSET_80 = :QFDB-DOFFSET-80,
              FDBFILE.FDB_DDIGITS_80 = :QFDB-DDIGITS-80,
              FDBFILE.FDB_DTYPE_80 = :QFDB-DTYPE-80,
              FDBFILE.FDB_DDECIMAL_80 = :QFDB-DDECIMAL-80,
              FDBFILE.FDB_DNAME_81 = :QFDB-DNAME-81,
              FDBFILE.FDB_DOFFSET_81 = :QFDB-DOFFSET-81,
              FDBFILE.FDB_DDIGITS_81 = :QFDB-DDIGITS-81,
              FDBFILE.FDB_DTYPE_81 = :QFDB-DTYPE-81,
              FDBFILE.FDB_DDECIMAL_81 = :QFDB-DDECIMAL-81,
              FDBFILE.FDB_DNAME_82 = :QFDB-DNAME-82,
              FDBFILE.FDB_DOFFSET_82 = :QFDB-DOFFSET-82,
              FDBFILE.FDB_DDIGITS_82 = :QFDB-DDIGITS-82,
              FDBFILE.FDB_DTYPE_82 = :QFDB-DTYPE-82,
              FDBFILE.FDB_DDECIMAL_82 = :QFDB-DDECIMAL-82,
              FDBFILE.FDB_DNAME_83 = :QFDB-DNAME-83,
              FDBFILE.FDB_DOFFSET_83 = :QFDB-DOFFSET-83,
              FDBFILE.FDB_DDIGITS_83 = :QFDB-DDIGITS-83,
              FDBFILE.FDB_DTYPE_83 = :QFDB-DTYPE-83,
              FDBFILE.FDB_DDECIMAL_83 = :QFDB-DDECIMAL-83,
              FDBFILE.FDB_DNAME_84 = :QFDB-DNAME-84,
              FDBFILE.FDB_DOFFSET_84 = :QFDB-DOFFSET-84,
              FDBFILE.FDB_DDIGITS_84 = :QFDB-DDIGITS-84,
              FDBFILE.FDB_DTYPE_84 = :QFDB-DTYPE-84,
              FDBFILE.FDB_DDECIMAL_84 = :QFDB-DDECIMAL-84,
              FDBFILE.FDB_DNAME_85 = :QFDB-DNAME-85,
              FDBFILE.FDB_DOFFSET_85 = :QFDB-DOFFSET-85,
              FDBFILE.FDB_DDIGITS_85 = :QFDB-DDIGITS-85,
              FDBFILE.FDB_DTYPE_85 = :QFDB-DTYPE-85,
              FDBFILE.FDB_DDECIMAL_85 = :QFDB-DDECIMAL-85,
              FDBFILE.FDB_DNAME_86 = :QFDB-DNAME-86,
              FDBFILE.FDB_DOFFSET_86 = :QFDB-DOFFSET-86,
              FDBFILE.FDB_DDIGITS_86 = :QFDB-DDIGITS-86,
              FDBFILE.FDB_DTYPE_86 = :QFDB-DTYPE-86,
              FDBFILE.FDB_DDECIMAL_86 = :QFDB-DDECIMAL-86,
              FDBFILE.FDB_DNAME_87 = :QFDB-DNAME-87,
              FDBFILE.FDB_DOFFSET_87 = :QFDB-DOFFSET-87,
              FDBFILE.FDB_DDIGITS_87 = :QFDB-DDIGITS-87,
              FDBFILE.FDB_DTYPE_87 = :QFDB-DTYPE-87,
              FDBFILE.FDB_DDECIMAL_87 = :QFDB-DDECIMAL-87,
              FDBFILE.FDB_DNAME_88 = :QFDB-DNAME-88,
              FDBFILE.FDB_DOFFSET_88 = :QFDB-DOFFSET-88,
              FDBFILE.FDB_DDIGITS_88 = :QFDB-DDIGITS-88,
              FDBFILE.FDB_DTYPE_88 = :QFDB-DTYPE-88,
              FDBFILE.FDB_DDECIMAL_88 = :QFDB-DDECIMAL-88,
              FDBFILE.FDB_DNAME_89 = :QFDB-DNAME-89,
              FDBFILE.FDB_DOFFSET_89 = :QFDB-DOFFSET-89,
              FDBFILE.FDB_DDIGITS_89 = :QFDB-DDIGITS-89,
              FDBFILE.FDB_DTYPE_89 = :QFDB-DTYPE-89,
              FDBFILE.FDB_DDECIMAL_89 = :QFDB-DDECIMAL-89,
              FDBFILE.FDB_DNAME_90 = :QFDB-DNAME-90,
              FDBFILE.FDB_DOFFSET_90 = :QFDB-DOFFSET-90,
              FDBFILE.FDB_DDIGITS_90 = :QFDB-DDIGITS-90,
              FDBFILE.FDB_DTYPE_90 = :QFDB-DTYPE-90,
              FDBFILE.FDB_DDECIMAL_90 = :QFDB-DDECIMAL-90,
              FDBFILE.FDB_DNAME_91 = :QFDB-DNAME-91,
              FDBFILE.FDB_DOFFSET_91 = :QFDB-DOFFSET-91,
              FDBFILE.FDB_DDIGITS_91 = :QFDB-DDIGITS-91,
              FDBFILE.FDB_DTYPE_91 = :QFDB-DTYPE-91,
              FDBFILE.FDB_DDECIMAL_91 = :QFDB-DDECIMAL-91,
              FDBFILE.FDB_DNAME_92 = :QFDB-DNAME-92,
              FDBFILE.FDB_DOFFSET_92 = :QFDB-DOFFSET-92,
              FDBFILE.FDB_DDIGITS_92 = :QFDB-DDIGITS-92,
              FDBFILE.FDB_DTYPE_92 = :QFDB-DTYPE-92,
              FDBFILE.FDB_DDECIMAL_92 = :QFDB-DDECIMAL-92,
              FDBFILE.FDB_DNAME_93 = :QFDB-DNAME-93,
              FDBFILE.FDB_DOFFSET_93 = :QFDB-DOFFSET-93,
              FDBFILE.FDB_DDIGITS_93 = :QFDB-DDIGITS-93,
              FDBFILE.FDB_DTYPE_93 = :QFDB-DTYPE-93,
              FDBFILE.FDB_DDECIMAL_93 = :QFDB-DDECIMAL-93,
              FDBFILE.FDB_DNAME_94 = :QFDB-DNAME-94,
              FDBFILE.FDB_DOFFSET_94 = :QFDB-DOFFSET-94,
              FDBFILE.FDB_DDIGITS_94 = :QFDB-DDIGITS-94,
              FDBFILE.FDB_DTYPE_94 = :QFDB-DTYPE-94,
              FDBFILE.FDB_DDECIMAL_94 = :QFDB-DDECIMAL-94,
              FDBFILE.FDB_DNAME_95 = :QFDB-DNAME-95,
              FDBFILE.FDB_DOFFSET_95 = :QFDB-DOFFSET-95,
              FDBFILE.FDB_DDIGITS_95 = :QFDB-DDIGITS-95,
              FDBFILE.FDB_DTYPE_95 = :QFDB-DTYPE-95,
              FDBFILE.FDB_DDECIMAL_95 = :QFDB-DDECIMAL-95,
              FDBFILE.FDB_DNAME_96 = :QFDB-DNAME-96,
              FDBFILE.FDB_DOFFSET_96 = :QFDB-DOFFSET-96,
              FDBFILE.FDB_DDIGITS_96 = :QFDB-DDIGITS-96,
              FDBFILE.FDB_DTYPE_96 = :QFDB-DTYPE-96,
              FDBFILE.FDB_DDECIMAL_96 = :QFDB-DDECIMAL-96,
              FDBFILE.FDB_DNAME_97 = :QFDB-DNAME-97,
              FDBFILE.FDB_DOFFSET_97 = :QFDB-DOFFSET-97,
              FDBFILE.FDB_DDIGITS_97 = :QFDB-DDIGITS-97,
              FDBFILE.FDB_DTYPE_97 = :QFDB-DTYPE-97,
              FDBFILE.FDB_DDECIMAL_97 = :QFDB-DDECIMAL-97,
              FDBFILE.FDB_DNAME_98 = :QFDB-DNAME-98,
              FDBFILE.FDB_DOFFSET_98 = :QFDB-DOFFSET-98,
              FDBFILE.FDB_DDIGITS_98 = :QFDB-DDIGITS-98,
              FDBFILE.FDB_DTYPE_98 = :QFDB-DTYPE-98,
              FDBFILE.FDB_DDECIMAL_98 = :QFDB-DDECIMAL-98,
              FDBFILE.FDB_DNAME_99 = :QFDB-DNAME-99,
              FDBFILE.FDB_DOFFSET_99 = :QFDB-DOFFSET-99,
              FDBFILE.FDB_DDIGITS_99 = :QFDB-DDIGITS-99,
              FDBFILE.FDB_DTYPE_99 = :QFDB-DTYPE-99,
              FDBFILE.FDB_DDECIMAL_99 = :QFDB-DDECIMAL-99,
              FDBFILE.FDB_DNAME_100 = :QFDB-DNAME-100,
              FDBFILE.FDB_DOFFSET_100 = :QFDB-DOFFSET-100,
              FDBFILE.FDB_DDIGITS_100 = :QFDB-DDIGITS-100,
              FDBFILE.FDB_DTYPE_100 = :QFDB-DTYPE-100,
              FDBFILE.FDB_DDECIMAL_100 = :QFDB-DDECIMAL-100,
              FDBFILE.FDB_DNAME_101 = :QFDB-DNAME-101,
              FDBFILE.FDB_DOFFSET_101 = :QFDB-DOFFSET-101,
              FDBFILE.FDB_DDIGITS_101 = :QFDB-DDIGITS-101,
              FDBFILE.FDB_DTYPE_101 = :QFDB-DTYPE-101,
              FDBFILE.FDB_DDECIMAL_101 = :QFDB-DDECIMAL-101,
              FDBFILE.FDB_DNAME_102 = :QFDB-DNAME-102,
              FDBFILE.FDB_DOFFSET_102 = :QFDB-DOFFSET-102,
              FDBFILE.FDB_DDIGITS_102 = :QFDB-DDIGITS-102,
              FDBFILE.FDB_DTYPE_102 = :QFDB-DTYPE-102,
              FDBFILE.FDB_DDECIMAL_102 = :QFDB-DDECIMAL-102,
              FDBFILE.FDB_DNAME_103 = :QFDB-DNAME-103,
              FDBFILE.FDB_DOFFSET_103 = :QFDB-DOFFSET-103,
              FDBFILE.FDB_DDIGITS_103 = :QFDB-DDIGITS-103,
              FDBFILE.FDB_DTYPE_103 = :QFDB-DTYPE-103,
              FDBFILE.FDB_DDECIMAL_103 = :QFDB-DDECIMAL-103,
              FDBFILE.FDB_DNAME_104 = :QFDB-DNAME-104,
              FDBFILE.FDB_DOFFSET_104 = :QFDB-DOFFSET-104,
              FDBFILE.FDB_DDIGITS_104 = :QFDB-DDIGITS-104,
              FDBFILE.FDB_DTYPE_104 = :QFDB-DTYPE-104,
              FDBFILE.FDB_DDECIMAL_104 = :QFDB-DDECIMAL-104,
              FDBFILE.FDB_DNAME_105 = :QFDB-DNAME-105,
              FDBFILE.FDB_DOFFSET_105 = :QFDB-DOFFSET-105,
              FDBFILE.FDB_DDIGITS_105 = :QFDB-DDIGITS-105,
              FDBFILE.FDB_DTYPE_105 = :QFDB-DTYPE-105,
              FDBFILE.FDB_DDECIMAL_105 = :QFDB-DDECIMAL-105,
              FDBFILE.FDB_DNAME_106 = :QFDB-DNAME-106,
              FDBFILE.FDB_DOFFSET_106 = :QFDB-DOFFSET-106,
              FDBFILE.FDB_DDIGITS_106 = :QFDB-DDIGITS-106,
              FDBFILE.FDB_DTYPE_106 = :QFDB-DTYPE-106,
              FDBFILE.FDB_DDECIMAL_106 = :QFDB-DDECIMAL-106,
              FDBFILE.FDB_DNAME_107 = :QFDB-DNAME-107,
              FDBFILE.FDB_DOFFSET_107 = :QFDB-DOFFSET-107,
              FDBFILE.FDB_DDIGITS_107 = :QFDB-DDIGITS-107,
              FDBFILE.FDB_DTYPE_107 = :QFDB-DTYPE-107,
              FDBFILE.FDB_DDECIMAL_107 = :QFDB-DDECIMAL-107,
              FDBFILE.FDB_DNAME_108 = :QFDB-DNAME-108,
              FDBFILE.FDB_DOFFSET_108 = :QFDB-DOFFSET-108,
              FDBFILE.FDB_DDIGITS_108 = :QFDB-DDIGITS-108,
              FDBFILE.FDB_DTYPE_108 = :QFDB-DTYPE-108,
              FDBFILE.FDB_DDECIMAL_108 = :QFDB-DDECIMAL-108,
              FDBFILE.FDB_DNAME_109 = :QFDB-DNAME-109,
              FDBFILE.FDB_DOFFSET_109 = :QFDB-DOFFSET-109,
              FDBFILE.FDB_DDIGITS_109 = :QFDB-DDIGITS-109,
              FDBFILE.FDB_DTYPE_109 = :QFDB-DTYPE-109,
              FDBFILE.FDB_DDECIMAL_109 = :QFDB-DDECIMAL-109,
              FDBFILE.FDB_DNAME_110 = :QFDB-DNAME-110,
              FDBFILE.FDB_DOFFSET_110 = :QFDB-DOFFSET-110,
              FDBFILE.FDB_DDIGITS_110 = :QFDB-DDIGITS-110,
              FDBFILE.FDB_DTYPE_110 = :QFDB-DTYPE-110,
              FDBFILE.FDB_DDECIMAL_110 = :QFDB-DDECIMAL-110,
              FDBFILE.FDB_DNAME_111 = :QFDB-DNAME-111,
              FDBFILE.FDB_DOFFSET_111 = :QFDB-DOFFSET-111,
              FDBFILE.FDB_DDIGITS_111 = :QFDB-DDIGITS-111,
              FDBFILE.FDB_DTYPE_111 = :QFDB-DTYPE-111,
              FDBFILE.FDB_DDECIMAL_111 = :QFDB-DDECIMAL-111,
              FDBFILE.FDB_DNAME_112 = :QFDB-DNAME-112,
              FDBFILE.FDB_DOFFSET_112 = :QFDB-DOFFSET-112,
              FDBFILE.FDB_DDIGITS_112 = :QFDB-DDIGITS-112,
              FDBFILE.FDB_DTYPE_112 = :QFDB-DTYPE-112,
              FDBFILE.FDB_DDECIMAL_112 = :QFDB-DDECIMAL-112,
              FDBFILE.FDB_DNAME_113 = :QFDB-DNAME-113,
              FDBFILE.FDB_DOFFSET_113 = :QFDB-DOFFSET-113,
              FDBFILE.FDB_DDIGITS_113 = :QFDB-DDIGITS-113,
              FDBFILE.FDB_DTYPE_113 = :QFDB-DTYPE-113,
              FDBFILE.FDB_DDECIMAL_113 = :QFDB-DDECIMAL-113,
              FDBFILE.FDB_DNAME_114 = :QFDB-DNAME-114,
              FDBFILE.FDB_DOFFSET_114 = :QFDB-DOFFSET-114,
              FDBFILE.FDB_DDIGITS_114 = :QFDB-DDIGITS-114,
              FDBFILE.FDB_DTYPE_114 = :QFDB-DTYPE-114,
              FDBFILE.FDB_DDECIMAL_114 = :QFDB-DDECIMAL-114,
              FDBFILE.FDB_DNAME_115 = :QFDB-DNAME-115,
              FDBFILE.FDB_DOFFSET_115 = :QFDB-DOFFSET-115,
              FDBFILE.FDB_DDIGITS_115 = :QFDB-DDIGITS-115,
              FDBFILE.FDB_DTYPE_115 = :QFDB-DTYPE-115,
              FDBFILE.FDB_DDECIMAL_115 = :QFDB-DDECIMAL-115,
              FDBFILE.FDB_DNAME_116 = :QFDB-DNAME-116,
              FDBFILE.FDB_DOFFSET_116 = :QFDB-DOFFSET-116,
              FDBFILE.FDB_DDIGITS_116 = :QFDB-DDIGITS-116,
              FDBFILE.FDB_DTYPE_116 = :QFDB-DTYPE-116,
              FDBFILE.FDB_DDECIMAL_116 = :QFDB-DDECIMAL-116,
              FDBFILE.FDB_DNAME_117 = :QFDB-DNAME-117,
              FDBFILE.FDB_DOFFSET_117 = :QFDB-DOFFSET-117,
              FDBFILE.FDB_DDIGITS_117 = :QFDB-DDIGITS-117,
              FDBFILE.FDB_DTYPE_117 = :QFDB-DTYPE-117,
              FDBFILE.FDB_DDECIMAL_117 = :QFDB-DDECIMAL-117,
              FDBFILE.FDB_DNAME_118 = :QFDB-DNAME-118,
              FDBFILE.FDB_DOFFSET_118 = :QFDB-DOFFSET-118,
              FDBFILE.FDB_DDIGITS_118 = :QFDB-DDIGITS-118,
              FDBFILE.FDB_DTYPE_118 = :QFDB-DTYPE-118,
              FDBFILE.FDB_DDECIMAL_118 = :QFDB-DDECIMAL-118,
              FDBFILE.FDB_DNAME_119 = :QFDB-DNAME-119,
              FDBFILE.FDB_DOFFSET_119 = :QFDB-DOFFSET-119,
              FDBFILE.FDB_DDIGITS_119 = :QFDB-DDIGITS-119,
              FDBFILE.FDB_DTYPE_119 = :QFDB-DTYPE-119,
              FDBFILE.FDB_DDECIMAL_119 = :QFDB-DDECIMAL-119,
              FDBFILE.FDB_DNAME_120 = :QFDB-DNAME-120,
              FDBFILE.FDB_DOFFSET_120 = :QFDB-DOFFSET-120,
              FDBFILE.FDB_DDIGITS_120 = :QFDB-DDIGITS-120,
              FDBFILE.FDB_DTYPE_120 = :QFDB-DTYPE-120,
              FDBFILE.FDB_DDECIMAL_120 = :QFDB-DDECIMAL-120,
              FDBFILE.FDB_DNAME_121 = :QFDB-DNAME-121,
              FDBFILE.FDB_DOFFSET_121 = :QFDB-DOFFSET-121,
              FDBFILE.FDB_DDIGITS_121 = :QFDB-DDIGITS-121,
              FDBFILE.FDB_DTYPE_121 = :QFDB-DTYPE-121,
              FDBFILE.FDB_DDECIMAL_121 = :QFDB-DDECIMAL-121,
              FDBFILE.FDB_DNAME_122 = :QFDB-DNAME-122,
              FDBFILE.FDB_DOFFSET_122 = :QFDB-DOFFSET-122,
              FDBFILE.FDB_DDIGITS_122 = :QFDB-DDIGITS-122,
              FDBFILE.FDB_DTYPE_122 = :QFDB-DTYPE-122,
              FDBFILE.FDB_DDECIMAL_122 = :QFDB-DDECIMAL-122,
              FDBFILE.FDB_DNAME_123 = :QFDB-DNAME-123,
              FDBFILE.FDB_DOFFSET_123 = :QFDB-DOFFSET-123,
              FDBFILE.FDB_DDIGITS_123 = :QFDB-DDIGITS-123,
              FDBFILE.FDB_DTYPE_123 = :QFDB-DTYPE-123,
              FDBFILE.FDB_DDECIMAL_123 = :QFDB-DDECIMAL-123,
              FDBFILE.FDB_DNAME_124 = :QFDB-DNAME-124,
              FDBFILE.FDB_DOFFSET_124 = :QFDB-DOFFSET-124,
              FDBFILE.FDB_DDIGITS_124 = :QFDB-DDIGITS-124,
              FDBFILE.FDB_DTYPE_124 = :QFDB-DTYPE-124,
              FDBFILE.FDB_DDECIMAL_124 = :QFDB-DDECIMAL-124,
              FDBFILE.FDB_DNAME_125 = :QFDB-DNAME-125,
              FDBFILE.FDB_DOFFSET_125 = :QFDB-DOFFSET-125,
              FDBFILE.FDB_DDIGITS_125 = :QFDB-DDIGITS-125,
              FDBFILE.FDB_DTYPE_125 = :QFDB-DTYPE-125,
              FDBFILE.FDB_DDECIMAL_125 = :QFDB-DDECIMAL-125,
              FDBFILE.FDB_DNAME_126 = :QFDB-DNAME-126,
              FDBFILE.FDB_DOFFSET_126 = :QFDB-DOFFSET-126,
              FDBFILE.FDB_DDIGITS_126 = :QFDB-DDIGITS-126,
              FDBFILE.FDB_DTYPE_126 = :QFDB-DTYPE-126,
              FDBFILE.FDB_DDECIMAL_126 = :QFDB-DDECIMAL-126,
              FDBFILE.FDB_DNAME_127 = :QFDB-DNAME-127,
              FDBFILE.FDB_DOFFSET_127 = :QFDB-DOFFSET-127,
              FDBFILE.FDB_DDIGITS_127 = :QFDB-DDIGITS-127,
              FDBFILE.FDB_DTYPE_127 = :QFDB-DTYPE-127,
              FDBFILE.FDB_DDECIMAL_127 = :QFDB-DDECIMAL-127,
              FDBFILE.FDB_DNAME_128 = :QFDB-DNAME-128,
              FDBFILE.FDB_DOFFSET_128 = :QFDB-DOFFSET-128,
              FDBFILE.FDB_DDIGITS_128 = :QFDB-DDIGITS-128,
              FDBFILE.FDB_DTYPE_128 = :QFDB-DTYPE-128,
              FDBFILE.FDB_DDECIMAL_128 = :QFDB-DDECIMAL-128,
              FDBFILE.FDB_DNAME_129 = :QFDB-DNAME-129,
              FDBFILE.FDB_DOFFSET_129 = :QFDB-DOFFSET-129,
              FDBFILE.FDB_DDIGITS_129 = :QFDB-DDIGITS-129,
              FDBFILE.FDB_DTYPE_129 = :QFDB-DTYPE-129,
              FDBFILE.FDB_DDECIMAL_129 = :QFDB-DDECIMAL-129,
              FDBFILE.FDB_DNAME_130 = :QFDB-DNAME-130,
              FDBFILE.FDB_DOFFSET_130 = :QFDB-DOFFSET-130,
              FDBFILE.FDB_DDIGITS_130 = :QFDB-DDIGITS-130,
              FDBFILE.FDB_DTYPE_130 = :QFDB-DTYPE-130,
              FDBFILE.FDB_DDECIMAL_130 = :QFDB-DDECIMAL-130,
              FDBFILE.FDB_DNAME_131 = :QFDB-DNAME-131,
              FDBFILE.FDB_DOFFSET_131 = :QFDB-DOFFSET-131,
              FDBFILE.FDB_DDIGITS_131 = :QFDB-DDIGITS-131,
              FDBFILE.FDB_DTYPE_131 = :QFDB-DTYPE-131,
              FDBFILE.FDB_DDECIMAL_131 = :QFDB-DDECIMAL-131,
              FDBFILE.FDB_DNAME_132 = :QFDB-DNAME-132,
              FDBFILE.FDB_DOFFSET_132 = :QFDB-DOFFSET-132,
              FDBFILE.FDB_DDIGITS_132 = :QFDB-DDIGITS-132,
              FDBFILE.FDB_DTYPE_132 = :QFDB-DTYPE-132,
              FDBFILE.FDB_DDECIMAL_132 = :QFDB-DDECIMAL-132,
              FDBFILE.FDB_DNAME_133 = :QFDB-DNAME-133,
              FDBFILE.FDB_DOFFSET_133 = :QFDB-DOFFSET-133,
              FDBFILE.FDB_DDIGITS_133 = :QFDB-DDIGITS-133,
              FDBFILE.FDB_DTYPE_133 = :QFDB-DTYPE-133,
              FDBFILE.FDB_DDECIMAL_133 = :QFDB-DDECIMAL-133,
              FDBFILE.FDB_DNAME_134 = :QFDB-DNAME-134,
              FDBFILE.FDB_DOFFSET_134 = :QFDB-DOFFSET-134,
              FDBFILE.FDB_DDIGITS_134 = :QFDB-DDIGITS-134,
              FDBFILE.FDB_DTYPE_134 = :QFDB-DTYPE-134,
              FDBFILE.FDB_DDECIMAL_134 = :QFDB-DDECIMAL-134,
              FDBFILE.FDB_DNAME_135 = :QFDB-DNAME-135,
              FDBFILE.FDB_DOFFSET_135 = :QFDB-DOFFSET-135,
              FDBFILE.FDB_DDIGITS_135 = :QFDB-DDIGITS-135,
              FDBFILE.FDB_DTYPE_135 = :QFDB-DTYPE-135,
              FDBFILE.FDB_DDECIMAL_135 = :QFDB-DDECIMAL-135,
              FDBFILE.FDB_DNAME_136 = :QFDB-DNAME-136,
              FDBFILE.FDB_DOFFSET_136 = :QFDB-DOFFSET-136,
              FDBFILE.FDB_DDIGITS_136 = :QFDB-DDIGITS-136,
              FDBFILE.FDB_DTYPE_136 = :QFDB-DTYPE-136,
              FDBFILE.FDB_DDECIMAL_136 = :QFDB-DDECIMAL-136,
              FDBFILE.FDB_DNAME_137 = :QFDB-DNAME-137,
              FDBFILE.FDB_DOFFSET_137 = :QFDB-DOFFSET-137,
              FDBFILE.FDB_DDIGITS_137 = :QFDB-DDIGITS-137,
              FDBFILE.FDB_DTYPE_137 = :QFDB-DTYPE-137,
              FDBFILE.FDB_DDECIMAL_137 = :QFDB-DDECIMAL-137,
              FDBFILE.FDB_DNAME_138 = :QFDB-DNAME-138,
              FDBFILE.FDB_DOFFSET_138 = :QFDB-DOFFSET-138,
              FDBFILE.FDB_DDIGITS_138 = :QFDB-DDIGITS-138,
              FDBFILE.FDB_DTYPE_138 = :QFDB-DTYPE-138,
              FDBFILE.FDB_DDECIMAL_138 = :QFDB-DDECIMAL-138,
              FDBFILE.FDB_DNAME_139 = :QFDB-DNAME-139,
              FDBFILE.FDB_DOFFSET_139 = :QFDB-DOFFSET-139,
              FDBFILE.FDB_DDIGITS_139 = :QFDB-DDIGITS-139,
              FDBFILE.FDB_DTYPE_139 = :QFDB-DTYPE-139,
              FDBFILE.FDB_DDECIMAL_139 = :QFDB-DDECIMAL-139,
              FDBFILE.FDB_DNAME_140 = :QFDB-DNAME-140,
              FDBFILE.FDB_DOFFSET_140 = :QFDB-DOFFSET-140,
              FDBFILE.FDB_DDIGITS_140 = :QFDB-DDIGITS-140,
              FDBFILE.FDB_DTYPE_140 = :QFDB-DTYPE-140,
              FDBFILE.FDB_DDECIMAL_140 = :QFDB-DDECIMAL-140,
              FDBFILE.FDB_DNAME_141 = :QFDB-DNAME-141,
              FDBFILE.FDB_DOFFSET_141 = :QFDB-DOFFSET-141,
              FDBFILE.FDB_DDIGITS_141 = :QFDB-DDIGITS-141,
              FDBFILE.FDB_DTYPE_141 = :QFDB-DTYPE-141,
              FDBFILE.FDB_DDECIMAL_141 = :QFDB-DDECIMAL-141,
              FDBFILE.FDB_DNAME_142 = :QFDB-DNAME-142,
              FDBFILE.FDB_DOFFSET_142 = :QFDB-DOFFSET-142,
              FDBFILE.FDB_DDIGITS_142 = :QFDB-DDIGITS-142,
              FDBFILE.FDB_DTYPE_142 = :QFDB-DTYPE-142,
              FDBFILE.FDB_DDECIMAL_142 = :QFDB-DDECIMAL-142,
              FDBFILE.FDB_DNAME_143 = :QFDB-DNAME-143,
              FDBFILE.FDB_DOFFSET_143 = :QFDB-DOFFSET-143,
              FDBFILE.FDB_DDIGITS_143 = :QFDB-DDIGITS-143,
              FDBFILE.FDB_DTYPE_143 = :QFDB-DTYPE-143,
              FDBFILE.FDB_DDECIMAL_143 = :QFDB-DDECIMAL-143,
              FDBFILE.FDB_DNAME_144 = :QFDB-DNAME-144,
              FDBFILE.FDB_DOFFSET_144 = :QFDB-DOFFSET-144,
              FDBFILE.FDB_DDIGITS_144 = :QFDB-DDIGITS-144,
              FDBFILE.FDB_DTYPE_144 = :QFDB-DTYPE-144,
              FDBFILE.FDB_DDECIMAL_144 = :QFDB-DDECIMAL-144,
              FDBFILE.FDB_DNAME_145 = :QFDB-DNAME-145,
              FDBFILE.FDB_DOFFSET_145 = :QFDB-DOFFSET-145,
              FDBFILE.FDB_DDIGITS_145 = :QFDB-DDIGITS-145,
              FDBFILE.FDB_DTYPE_145 = :QFDB-DTYPE-145,
              FDBFILE.FDB_DDECIMAL_145 = :QFDB-DDECIMAL-145,
              FDBFILE.FDB_DNAME_146 = :QFDB-DNAME-146,
              FDBFILE.FDB_DOFFSET_146 = :QFDB-DOFFSET-146,
              FDBFILE.FDB_DDIGITS_146 = :QFDB-DDIGITS-146,
              FDBFILE.FDB_DTYPE_146 = :QFDB-DTYPE-146,
              FDBFILE.FDB_DDECIMAL_146 = :QFDB-DDECIMAL-146,
              FDBFILE.FDB_DNAME_147 = :QFDB-DNAME-147,
              FDBFILE.FDB_DOFFSET_147 = :QFDB-DOFFSET-147,
              FDBFILE.FDB_DDIGITS_147 = :QFDB-DDIGITS-147,
              FDBFILE.FDB_DTYPE_147 = :QFDB-DTYPE-147,
              FDBFILE.FDB_DDECIMAL_147 = :QFDB-DDECIMAL-147,
              FDBFILE.FDB_DNAME_148 = :QFDB-DNAME-148,
              FDBFILE.FDB_DOFFSET_148 = :QFDB-DOFFSET-148,
              FDBFILE.FDB_DDIGITS_148 = :QFDB-DDIGITS-148,
              FDBFILE.FDB_DTYPE_148 = :QFDB-DTYPE-148,
              FDBFILE.FDB_DDECIMAL_148 = :QFDB-DDECIMAL-148,
              FDBFILE.FDB_DNAME_149 = :QFDB-DNAME-149,
              FDBFILE.FDB_DOFFSET_149 = :QFDB-DOFFSET-149,
              FDBFILE.FDB_DDIGITS_149 = :QFDB-DDIGITS-149,
              FDBFILE.FDB_DTYPE_149 = :QFDB-DTYPE-149,
              FDBFILE.FDB_DDECIMAL_149 = :QFDB-DDECIMAL-149,
              FDBFILE.FDB_DNAME_150 = :QFDB-DNAME-150,
              FDBFILE.FDB_DOFFSET_150 = :QFDB-DOFFSET-150,
              FDBFILE.FDB_DDIGITS_150 = :QFDB-DDIGITS-150,
              FDBFILE.FDB_DTYPE_150 = :QFDB-DTYPE-150,
              FDBFILE.FDB_DDECIMAL_150 = :QFDB-DDECIMAL-150,
              FDBFILE.FDB_DNAME_151 = :QFDB-DNAME-151,
              FDBFILE.FDB_DOFFSET_151 = :QFDB-DOFFSET-151,
              FDBFILE.FDB_DDIGITS_151 = :QFDB-DDIGITS-151,
              FDBFILE.FDB_DTYPE_151 = :QFDB-DTYPE-151,
              FDBFILE.FDB_DDECIMAL_151 = :QFDB-DDECIMAL-151,
              FDBFILE.FDB_DNAME_152 = :QFDB-DNAME-152,
              FDBFILE.FDB_DOFFSET_152 = :QFDB-DOFFSET-152,
              FDBFILE.FDB_DDIGITS_152 = :QFDB-DDIGITS-152,
              FDBFILE.FDB_DTYPE_152 = :QFDB-DTYPE-152,
              FDBFILE.FDB_DDECIMAL_152 = :QFDB-DDECIMAL-152,
              FDBFILE.FDB_DNAME_153 = :QFDB-DNAME-153,
              FDBFILE.FDB_DOFFSET_153 = :QFDB-DOFFSET-153,
              FDBFILE.FDB_DDIGITS_153 = :QFDB-DDIGITS-153,
              FDBFILE.FDB_DTYPE_153 = :QFDB-DTYPE-153,
              FDBFILE.FDB_DDECIMAL_153 = :QFDB-DDECIMAL-153,
              FDBFILE.FDB_DNAME_154 = :QFDB-DNAME-154,
              FDBFILE.FDB_DOFFSET_154 = :QFDB-DOFFSET-154,
              FDBFILE.FDB_DDIGITS_154 = :QFDB-DDIGITS-154,
              FDBFILE.FDB_DTYPE_154 = :QFDB-DTYPE-154,
              FDBFILE.FDB_DDECIMAL_154 = :QFDB-DDECIMAL-154,
              FDBFILE.FDB_DNAME_155 = :QFDB-DNAME-155,
              FDBFILE.FDB_DOFFSET_155 = :QFDB-DOFFSET-155,
              FDBFILE.FDB_DDIGITS_155 = :QFDB-DDIGITS-155,
              FDBFILE.FDB_DTYPE_155 = :QFDB-DTYPE-155,
              FDBFILE.FDB_DDECIMAL_155 = :QFDB-DDECIMAL-155,
              FDBFILE.FDB_DNAME_156 = :QFDB-DNAME-156,
              FDBFILE.FDB_DOFFSET_156 = :QFDB-DOFFSET-156,
              FDBFILE.FDB_DDIGITS_156 = :QFDB-DDIGITS-156,
              FDBFILE.FDB_DTYPE_156 = :QFDB-DTYPE-156,
              FDBFILE.FDB_DDECIMAL_156 = :QFDB-DDECIMAL-156,
              FDBFILE.FDB_DNAME_157 = :QFDB-DNAME-157,
              FDBFILE.FDB_DOFFSET_157 = :QFDB-DOFFSET-157,
              FDBFILE.FDB_DDIGITS_157 = :QFDB-DDIGITS-157,
              FDBFILE.FDB_DTYPE_157 = :QFDB-DTYPE-157,
              FDBFILE.FDB_DDECIMAL_157 = :QFDB-DDECIMAL-157,
              FDBFILE.FDB_DNAME_158 = :QFDB-DNAME-158,
              FDBFILE.FDB_DOFFSET_158 = :QFDB-DOFFSET-158,
              FDBFILE.FDB_DDIGITS_158 = :QFDB-DDIGITS-158,
              FDBFILE.FDB_DTYPE_158 = :QFDB-DTYPE-158,
              FDBFILE.FDB_DDECIMAL_158 = :QFDB-DDECIMAL-158,
              FDBFILE.FDB_DNAME_159 = :QFDB-DNAME-159,
              FDBFILE.FDB_DOFFSET_159 = :QFDB-DOFFSET-159,
              FDBFILE.FDB_DDIGITS_159 = :QFDB-DDIGITS-159,
              FDBFILE.FDB_DTYPE_159 = :QFDB-DTYPE-159,
              FDBFILE.FDB_DDECIMAL_159 = :QFDB-DDECIMAL-159,
              FDBFILE.FDB_DNAME_160 = :QFDB-DNAME-160,
              FDBFILE.FDB_DOFFSET_160 = :QFDB-DOFFSET-160,
              FDBFILE.FDB_DDIGITS_160 = :QFDB-DDIGITS-160,
              FDBFILE.FDB_DTYPE_160 = :QFDB-DTYPE-160,
              FDBFILE.FDB_DDECIMAL_160 = :QFDB-DDECIMAL-160,
              FDBFILE.FDB_DNAME_161 = :QFDB-DNAME-161,
              FDBFILE.FDB_DOFFSET_161 = :QFDB-DOFFSET-161,
              FDBFILE.FDB_DDIGITS_161 = :QFDB-DDIGITS-161,
              FDBFILE.FDB_DTYPE_161 = :QFDB-DTYPE-161,
              FDBFILE.FDB_DDECIMAL_161 = :QFDB-DDECIMAL-161,
              FDBFILE.FDB_DNAME_162 = :QFDB-DNAME-162,
              FDBFILE.FDB_DOFFSET_162 = :QFDB-DOFFSET-162,
              FDBFILE.FDB_DDIGITS_162 = :QFDB-DDIGITS-162,
              FDBFILE.FDB_DTYPE_162 = :QFDB-DTYPE-162,
              FDBFILE.FDB_DDECIMAL_162 = :QFDB-DDECIMAL-162,
              FDBFILE.FDB_DNAME_163 = :QFDB-DNAME-163,
              FDBFILE.FDB_DOFFSET_163 = :QFDB-DOFFSET-163,
              FDBFILE.FDB_DDIGITS_163 = :QFDB-DDIGITS-163,
              FDBFILE.FDB_DTYPE_163 = :QFDB-DTYPE-163,
              FDBFILE.FDB_DDECIMAL_163 = :QFDB-DDECIMAL-163,
              FDBFILE.FDB_DNAME_164 = :QFDB-DNAME-164,
              FDBFILE.FDB_DOFFSET_164 = :QFDB-DOFFSET-164,
              FDBFILE.FDB_DDIGITS_164 = :QFDB-DDIGITS-164,
              FDBFILE.FDB_DTYPE_164 = :QFDB-DTYPE-164,
              FDBFILE.FDB_DDECIMAL_164 = :QFDB-DDECIMAL-164,
              FDBFILE.FDB_DNAME_165 = :QFDB-DNAME-165,
              FDBFILE.FDB_DOFFSET_165 = :QFDB-DOFFSET-165,
              FDBFILE.FDB_DDIGITS_165 = :QFDB-DDIGITS-165,
              FDBFILE.FDB_DTYPE_165 = :QFDB-DTYPE-165,
              FDBFILE.FDB_DDECIMAL_165 = :QFDB-DDECIMAL-165,
              FDBFILE.FDB_DNAME_166 = :QFDB-DNAME-166,
              FDBFILE.FDB_DOFFSET_166 = :QFDB-DOFFSET-166,
              FDBFILE.FDB_DDIGITS_166 = :QFDB-DDIGITS-166,
              FDBFILE.FDB_DTYPE_166 = :QFDB-DTYPE-166,
              FDBFILE.FDB_DDECIMAL_166 = :QFDB-DDECIMAL-166,
              FDBFILE.FDB_DNAME_167 = :QFDB-DNAME-167,
              FDBFILE.FDB_DOFFSET_167 = :QFDB-DOFFSET-167,
              FDBFILE.FDB_DDIGITS_167 = :QFDB-DDIGITS-167,
              FDBFILE.FDB_DTYPE_167 = :QFDB-DTYPE-167,
              FDBFILE.FDB_DDECIMAL_167 = :QFDB-DDECIMAL-167,
              FDBFILE.FDB_DNAME_168 = :QFDB-DNAME-168,
              FDBFILE.FDB_DOFFSET_168 = :QFDB-DOFFSET-168,
              FDBFILE.FDB_DDIGITS_168 = :QFDB-DDIGITS-168,
              FDBFILE.FDB_DTYPE_168 = :QFDB-DTYPE-168,
              FDBFILE.FDB_DDECIMAL_168 = :QFDB-DDECIMAL-168,
              FDBFILE.FDB_DNAME_169 = :QFDB-DNAME-169,
              FDBFILE.FDB_DOFFSET_169 = :QFDB-DOFFSET-169,
              FDBFILE.FDB_DDIGITS_169 = :QFDB-DDIGITS-169,
              FDBFILE.FDB_DTYPE_169 = :QFDB-DTYPE-169,
              FDBFILE.FDB_DDECIMAL_169 = :QFDB-DDECIMAL-169,
              FDBFILE.FDB_DNAME_170 = :QFDB-DNAME-170,
              FDBFILE.FDB_DOFFSET_170 = :QFDB-DOFFSET-170,
              FDBFILE.FDB_DDIGITS_170 = :QFDB-DDIGITS-170,
              FDBFILE.FDB_DTYPE_170 = :QFDB-DTYPE-170,
              FDBFILE.FDB_DDECIMAL_170 = :QFDB-DDECIMAL-170,
              FDBFILE.FDB_DNAME_171 = :QFDB-DNAME-171,
              FDBFILE.FDB_DOFFSET_171 = :QFDB-DOFFSET-171,
              FDBFILE.FDB_DDIGITS_171 = :QFDB-DDIGITS-171,
              FDBFILE.FDB_DTYPE_171 = :QFDB-DTYPE-171,
              FDBFILE.FDB_DDECIMAL_171 = :QFDB-DDECIMAL-171,
              FDBFILE.FDB_DNAME_172 = :QFDB-DNAME-172,
              FDBFILE.FDB_DOFFSET_172 = :QFDB-DOFFSET-172,
              FDBFILE.FDB_DDIGITS_172 = :QFDB-DDIGITS-172,
              FDBFILE.FDB_DTYPE_172 = :QFDB-DTYPE-172,
              FDBFILE.FDB_DDECIMAL_172 = :QFDB-DDECIMAL-172,
              FDBFILE.FDB_DNAME_173 = :QFDB-DNAME-173,
              FDBFILE.FDB_DOFFSET_173 = :QFDB-DOFFSET-173,
              FDBFILE.FDB_DDIGITS_173 = :QFDB-DDIGITS-173,
              FDBFILE.FDB_DTYPE_173 = :QFDB-DTYPE-173,
              FDBFILE.FDB_DDECIMAL_173 = :QFDB-DDECIMAL-173,
              FDBFILE.FDB_DNAME_174 = :QFDB-DNAME-174,
              FDBFILE.FDB_DOFFSET_174 = :QFDB-DOFFSET-174,
              FDBFILE.FDB_DDIGITS_174 = :QFDB-DDIGITS-174,
              FDBFILE.FDB_DTYPE_174 = :QFDB-DTYPE-174,
              FDBFILE.FDB_DDECIMAL_174 = :QFDB-DDECIMAL-174,
              FDBFILE.FDB_DNAME_175 = :QFDB-DNAME-175,
              FDBFILE.FDB_DOFFSET_175 = :QFDB-DOFFSET-175,
              FDBFILE.FDB_DDIGITS_175 = :QFDB-DDIGITS-175,
              FDBFILE.FDB_DTYPE_175 = :QFDB-DTYPE-175,
              FDBFILE.FDB_DDECIMAL_175 = :QFDB-DDECIMAL-175,
              FDBFILE.FDB_DNAME_176 = :QFDB-DNAME-176,
              FDBFILE.FDB_DOFFSET_176 = :QFDB-DOFFSET-176,
              FDBFILE.FDB_DDIGITS_176 = :QFDB-DDIGITS-176,
              FDBFILE.FDB_DTYPE_176 = :QFDB-DTYPE-176,
              FDBFILE.FDB_DDECIMAL_176 = :QFDB-DDECIMAL-176,
              FDBFILE.FDB_DNAME_177 = :QFDB-DNAME-177,
              FDBFILE.FDB_DOFFSET_177 = :QFDB-DOFFSET-177,
              FDBFILE.FDB_DDIGITS_177 = :QFDB-DDIGITS-177,
              FDBFILE.FDB_DTYPE_177 = :QFDB-DTYPE-177,
              FDBFILE.FDB_DDECIMAL_177 = :QFDB-DDECIMAL-177,
              FDBFILE.FDB_DNAME_178 = :QFDB-DNAME-178,
              FDBFILE.FDB_DOFFSET_178 = :QFDB-DOFFSET-178,
              FDBFILE.FDB_DDIGITS_178 = :QFDB-DDIGITS-178,
              FDBFILE.FDB_DTYPE_178 = :QFDB-DTYPE-178,
              FDBFILE.FDB_DDECIMAL_178 = :QFDB-DDECIMAL-178,
              FDBFILE.FDB_DNAME_179 = :QFDB-DNAME-179,
              FDBFILE.FDB_DOFFSET_179 = :QFDB-DOFFSET-179,
              FDBFILE.FDB_DDIGITS_179 = :QFDB-DDIGITS-179,
              FDBFILE.FDB_DTYPE_179 = :QFDB-DTYPE-179,
              FDBFILE.FDB_DDECIMAL_179 = :QFDB-DDECIMAL-179,
              FDBFILE.FDB_DNAME_180 = :QFDB-DNAME-180,
              FDBFILE.FDB_DOFFSET_180 = :QFDB-DOFFSET-180,
              FDBFILE.FDB_DDIGITS_180 = :QFDB-DDIGITS-180,
              FDBFILE.FDB_DTYPE_180 = :QFDB-DTYPE-180,
              FDBFILE.FDB_DDECIMAL_180 = :QFDB-DDECIMAL-180,
              FDBFILE.FDB_DNAME_181 = :QFDB-DNAME-181,
              FDBFILE.FDB_DOFFSET_181 = :QFDB-DOFFSET-181,
              FDBFILE.FDB_DDIGITS_181 = :QFDB-DDIGITS-181,
              FDBFILE.FDB_DTYPE_181 = :QFDB-DTYPE-181,
              FDBFILE.FDB_DDECIMAL_181 = :QFDB-DDECIMAL-181,
              FDBFILE.FDB_DNAME_182 = :QFDB-DNAME-182,
              FDBFILE.FDB_DOFFSET_182 = :QFDB-DOFFSET-182,
              FDBFILE.FDB_DDIGITS_182 = :QFDB-DDIGITS-182,
              FDBFILE.FDB_DTYPE_182 = :QFDB-DTYPE-182,
              FDBFILE.FDB_DDECIMAL_182 = :QFDB-DDECIMAL-182,
              FDBFILE.FDB_DNAME_183 = :QFDB-DNAME-183,
              FDBFILE.FDB_DOFFSET_183 = :QFDB-DOFFSET-183,
              FDBFILE.FDB_DDIGITS_183 = :QFDB-DDIGITS-183,
              FDBFILE.FDB_DTYPE_183 = :QFDB-DTYPE-183,
              FDBFILE.FDB_DDECIMAL_183 = :QFDB-DDECIMAL-183,
              FDBFILE.FDB_DNAME_184 = :QFDB-DNAME-184,
              FDBFILE.FDB_DOFFSET_184 = :QFDB-DOFFSET-184,
              FDBFILE.FDB_DDIGITS_184 = :QFDB-DDIGITS-184,
              FDBFILE.FDB_DTYPE_184 = :QFDB-DTYPE-184,
              FDBFILE.FDB_DDECIMAL_184 = :QFDB-DDECIMAL-184,
              FDBFILE.FDB_DNAME_185 = :QFDB-DNAME-185,
              FDBFILE.FDB_DOFFSET_185 = :QFDB-DOFFSET-185,
              FDBFILE.FDB_DDIGITS_185 = :QFDB-DDIGITS-185,
              FDBFILE.FDB_DTYPE_185 = :QFDB-DTYPE-185,
              FDBFILE.FDB_DDECIMAL_185 = :QFDB-DDECIMAL-185,
              FDBFILE.FDB_DNAME_186 = :QFDB-DNAME-186,
              FDBFILE.FDB_DOFFSET_186 = :QFDB-DOFFSET-186,
              FDBFILE.FDB_DDIGITS_186 = :QFDB-DDIGITS-186,
              FDBFILE.FDB_DTYPE_186 = :QFDB-DTYPE-186,
              FDBFILE.FDB_DDECIMAL_186 = :QFDB-DDECIMAL-186,
              FDBFILE.FDB_DNAME_187 = :QFDB-DNAME-187,
              FDBFILE.FDB_DOFFSET_187 = :QFDB-DOFFSET-187,
              FDBFILE.FDB_DDIGITS_187 = :QFDB-DDIGITS-187,
              FDBFILE.FDB_DTYPE_187 = :QFDB-DTYPE-187,
              FDBFILE.FDB_DDECIMAL_187 = :QFDB-DDECIMAL-187,
              FDBFILE.FDB_DNAME_188 = :QFDB-DNAME-188,
              FDBFILE.FDB_DOFFSET_188 = :QFDB-DOFFSET-188,
              FDBFILE.FDB_DDIGITS_188 = :QFDB-DDIGITS-188,
              FDBFILE.FDB_DTYPE_188 = :QFDB-DTYPE-188,
              FDBFILE.FDB_DDECIMAL_188 = :QFDB-DDECIMAL-188,
              FDBFILE.FDB_DNAME_189 = :QFDB-DNAME-189,
              FDBFILE.FDB_DOFFSET_189 = :QFDB-DOFFSET-189,
              FDBFILE.FDB_DDIGITS_189 = :QFDB-DDIGITS-189,
              FDBFILE.FDB_DTYPE_189 = :QFDB-DTYPE-189,
              FDBFILE.FDB_DDECIMAL_189 = :QFDB-DDECIMAL-189,
              FDBFILE.FDB_DNAME_190 = :QFDB-DNAME-190,
              FDBFILE.FDB_DOFFSET_190 = :QFDB-DOFFSET-190,
              FDBFILE.FDB_DDIGITS_190 = :QFDB-DDIGITS-190,
              FDBFILE.FDB_DTYPE_190 = :QFDB-DTYPE-190,
              FDBFILE.FDB_DDECIMAL_190 = :QFDB-DDECIMAL-190,
              FDBFILE.FDB_DNAME_191 = :QFDB-DNAME-191,
              FDBFILE.FDB_DOFFSET_191 = :QFDB-DOFFSET-191,
              FDBFILE.FDB_DDIGITS_191 = :QFDB-DDIGITS-191,
              FDBFILE.FDB_DTYPE_191 = :QFDB-DTYPE-191,
              FDBFILE.FDB_DDECIMAL_191 = :QFDB-DDECIMAL-191,
              FDBFILE.FDB_DNAME_192 = :QFDB-DNAME-192,
              FDBFILE.FDB_DOFFSET_192 = :QFDB-DOFFSET-192,
              FDBFILE.FDB_DDIGITS_192 = :QFDB-DDIGITS-192,
              FDBFILE.FDB_DTYPE_192 = :QFDB-DTYPE-192,
              FDBFILE.FDB_DDECIMAL_192 = :QFDB-DDECIMAL-192,
              FDBFILE.FDB_DNAME_193 = :QFDB-DNAME-193,
              FDBFILE.FDB_DOFFSET_193 = :QFDB-DOFFSET-193,
              FDBFILE.FDB_DDIGITS_193 = :QFDB-DDIGITS-193,
              FDBFILE.FDB_DTYPE_193 = :QFDB-DTYPE-193,
              FDBFILE.FDB_DDECIMAL_193 = :QFDB-DDECIMAL-193,
              FDBFILE.FDB_DNAME_194 = :QFDB-DNAME-194,
              FDBFILE.FDB_DOFFSET_194 = :QFDB-DOFFSET-194,
              FDBFILE.FDB_DDIGITS_194 = :QFDB-DDIGITS-194,
              FDBFILE.FDB_DTYPE_194 = :QFDB-DTYPE-194,
              FDBFILE.FDB_DDECIMAL_194 = :QFDB-DDECIMAL-194,
              FDBFILE.FDB_DNAME_195 = :QFDB-DNAME-195,
              FDBFILE.FDB_DOFFSET_195 = :QFDB-DOFFSET-195,
              FDBFILE.FDB_DDIGITS_195 = :QFDB-DDIGITS-195,
              FDBFILE.FDB_DTYPE_195 = :QFDB-DTYPE-195,
              FDBFILE.FDB_DDECIMAL_195 = :QFDB-DDECIMAL-195,
              FDBFILE.FDB_DNAME_196 = :QFDB-DNAME-196,
              FDBFILE.FDB_DOFFSET_196 = :QFDB-DOFFSET-196,
              FDBFILE.FDB_DDIGITS_196 = :QFDB-DDIGITS-196,
              FDBFILE.FDB_DTYPE_196 = :QFDB-DTYPE-196,
              FDBFILE.FDB_DDECIMAL_196 = :QFDB-DDECIMAL-196,
              FDBFILE.FDB_DNAME_197 = :QFDB-DNAME-197,
              FDBFILE.FDB_DOFFSET_197 = :QFDB-DOFFSET-197,
              FDBFILE.FDB_DDIGITS_197 = :QFDB-DDIGITS-197,
              FDBFILE.FDB_DTYPE_197 = :QFDB-DTYPE-197,
              FDBFILE.FDB_DDECIMAL_197 = :QFDB-DDECIMAL-197,
              FDBFILE.FDB_DNAME_198 = :QFDB-DNAME-198,
              FDBFILE.FDB_DOFFSET_198 = :QFDB-DOFFSET-198,
              FDBFILE.FDB_DDIGITS_198 = :QFDB-DDIGITS-198,
              FDBFILE.FDB_DTYPE_198 = :QFDB-DTYPE-198,
              FDBFILE.FDB_DDECIMAL_198 = :QFDB-DDECIMAL-198,
              FDBFILE.FDB_DNAME_199 = :QFDB-DNAME-199,
              FDBFILE.FDB_DOFFSET_199 = :QFDB-DOFFSET-199,
              FDBFILE.FDB_DDIGITS_199 = :QFDB-DDIGITS-199,
              FDBFILE.FDB_DTYPE_199 = :QFDB-DTYPE-199,
              FDBFILE.FDB_DDECIMAL_199 = :QFDB-DDECIMAL-199,
              FDBFILE.FDB_DNAME_200 = :QFDB-DNAME-200,
              FDBFILE.FDB_DOFFSET_200 = :QFDB-DOFFSET-200,
              FDBFILE.FDB_DDIGITS_200 = :QFDB-DDIGITS-200,
              FDBFILE.FDB_DTYPE_200 = :QFDB-DTYPE-200,
              FDBFILE.FDB_DDECIMAL_200 = :QFDB-DDECIMAL-200,
              FDBFILE.FDB_CALLID_1 = :QFDB-CALLID-1,
              FDBFILE.FDB_KEYLOC_1 = :QFDB-KEYLOC-1,
              FDBFILE.FDB_CALLID_2 = :QFDB-CALLID-2,
              FDBFILE.FDB_KEYLOC_2 = :QFDB-KEYLOC-2
             WHERE FDBFILE.FDB_DBNO = :QFDB-DBNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-FDB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-FDB1-FILE.

           PERFORM DELETE-IT.
           MOVE FDB-SELECT-PATH TO E-FILE.
           MOVE FDB1-KEY TO E-KEYX.
           PERFORM SET-FDB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.FDBFILE
             WHERE FDBFILE.FDB_DBNO = :QFDB-DBNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-FDB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-FDB1-FILE.
           IF ( FDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE FDB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO FDB1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOFDBI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
