       IDENTIFICATION DIVISION.
       PROGRAM-ID. A3SCOMM.
       DATE-WRITTEN. date.
       AUTHOR. Fouzaan.
      *Description: this calculates and displays data of sales
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      * input-file declaration
           SELECT INPUT-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL.
      *
      * output-file declaration
           SELECT OUTPUT-FILE
               ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD INPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS INPUT-LINE
           RECORD CONTAINS 32 CHARACTERS.
      *
       01 INPUT-LINE.
          05 NOM                        PIC 9(3).
          05 NAME                       PIC X(8).
          05 SALES                      PIC 9(6).
          05 MIN-COMM                   PIC 9(6).
          05 MAX-COMM                   PIC 9(6).
          05 COMM                       PIC 9(2)V9.

        FD OUTPUT-FILE
            RECORDING MODE IS F
            DATA RECORD IS OUTPUT-LINE
            RECORD CONTAINS 150 CHARACTERS.

       01 OUTPUT-LINE                   PIC X(150).

         WORKING-STORAGE SECTION.
       01 WS-HEADER-LINE.
          05 FILLER                     PIC X(30) VALUE SPACES.
          05 FILLER                     PIC X(50) VALUE SPACES.
          05 FILLER                     PIC X(50) VALUE SPACES.
          05 FILLER                     PIC X(20) VALUE
                "Fouzaan ahmed, A2".

       01 WS-EMPTY-LINE.
          05 FILLER                     PIC X(80) VALUE SPACES.

       01 WS-COLUMN-HEADINGS.
          05 FILLER                     PIC X(5) VALUE "NO.".
          05 FILLER                     PIC X(15) VALUE "NAME".
          05 FILLER                     PIC X(15) VALUE "SALES".
          05 FILLER                     PIC X(10) VALUE "MIN".
          05 FILLER                     PIC X(10) VALUE "MAX".
          05 FILLER                     PIC X(8) VALUE "RATE".
          05 FILLER                     PIC X(11) VALUE "EARNED".
          05 FILLER                     PIC X(10) VALUE "PAID".
          05 FILLER                     PIC X(18) VALUE
                "BONUS/ NO BONUS".
          05 FILLER                     PIC X(1) VALUE SPACES.
       01 WS-TITLE-LINE.
          05 FILLER                     PIC X(55) VALUE SPACES.
          05 FILLER                     PIC X(40) VALUE
                "SALES COMMISION REPORT".
          05 FILLER                     PIC X(55) VALUE SPACES.
       01 WS-FLAGS.
          05 WS-EOF-FLAG                PIC X VALUE "n".
          05 WS-OTHER-FLAG              PIC X VALUE "x".

       01 WS-DEATIL-LINE.
          05 WS-NUMBER                  PIC 9(3).
          05 FILLER                     PIC X(2) VALUE SPACES.
          05 WS-NAME                    PIC X(8).
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-SALES                   PIC ZZZ,ZZ9.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-MIN                     PIC ZZZ,ZZ9.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-MAX                     PIC ZZZ,ZZ9.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-RATE                    PIC ZZ9.99.
          05 FILLER                     PIC X(4) VALUE SPACES.
          05 WS-EARNED                  PIC ZZZ,ZZ9.99.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-PAID                    PIC $ZZ,ZZ9.99.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-BONUS                   PIC X(20).
          05 FILLER                     PIC X(5) VALUE SPACES.
      *     Grand totals
       01 WS-GRAND-TOTALS.
          05 WS-TOTAL-TITLE             PIC X(60)
                VALUE "Totals   ".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-EARNED-TOTAL-VALUE      PIC $ZZZ,ZZ9.99.
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-PAID-TOTAL-VALUE        PIC $ZZZ,ZZ9.99.
      *     init the detail paragraph
       01 WS-BONUS-MORE-DETAIL.
          05 WS-BONUS-MORE-MAX          PIC X(60)
                VALUE "NUMBER WITH BONUS MORE THAN MAX".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-BONUS-MORE-MAX-VALUE    PIC 9(3).

       01 WS-BONUS-LESS-DETAIL.
          05 WS-BONUS-LESS-MAX          PIC X(60)
                VALUE "NUMBER WITH NO BONUS LESS THAN MIN".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-BONUS-LESS-MAX-VALUE    PIC 9(3).

       01 WS-TOTAL-BONUS-DETAIL.
          05 WS-TOTAL-BONUS-TEXT        PIC X(60)
                VALUE "NUMBER OF SALESPEOPLE WITH BONUS".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-TOTAL-BONUS-VALUE       PIC 9(3).

       01 WS-TOTAL-NO-BONUS-DETAIL.
          05 WS-TOTAL-NO-BONUS-TEXT     PIC X(60)
                VALUE "NUMBER OF SALESPEOPLE WITHOUT BONUS".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-TOTAL-NO-BONUS-VALUE    PIC 9(3).

       01 WS-TOTAL-SALESPERSONS-DETAIL.
          05 WS-TOTAL-SALESPERSONS-TEXT PIC X(60)
                VALUE "NUMBER OF SALESPEOPLE".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-TOTAL-SALESPERSONS-VALUE
                                        PIC 9(3).

       01 WS-TOTAL-PAID-EQUAL-DETAIL.
          05 WS-TOTAL-PAID-EQUAL-EARNED PIC X(60)
                VALUE "NUMBER WITH PAID EQUAL EARNED".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-TOTAL-CAL-EQUAL-EARNED  PIC 9(3).

       01 WS-TOTAL-PERCENT-DETAIL.
          05 WS-TOTAL-PERCENT-TEXT      PIC X(60)
                VALUE "PERCENT WITH PAID EQUAL EARNED".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-TOTAL-PERCENT-VALUE     PIC ZZ9V99.
       01 WS-BONUS-PERCENT-DETAIL.
          05 WS-BONUS-PERCENT-TEXT      PIC X(60)
                VALUE "PERCENT WITH BONUS     >300,000".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-BONUS-PERCENT-VALUE     PIC ZZ9V99.

       01 WS-NO-BONUS-PERCENT-DETAIL.
          05 WS-NO-BONUS-PERCENT-TEXT   PIC X(60)
                VALUE "PERCENT WITHOUT BONUS <=300,000".
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-NO-BONUS-PERCENT-VALUE  PIC ZZ9V99.


       01 WS-TOTAL-BONUS                PIC 9(3) VALUE 0.
       01 WS-TOTAL-NO-BONUS             PIC 9(3) VALUE 0.
       01 WS-TOTAL-PAID                 PIC 9(9)V99 VALUE 0.00.
       01 WS-TOTAL-PAID-EQUAL-TO-EARNED PIC 9(5)V9(2) VALUE 0.00.
       01 WS-TOTAL-SALESPERSONS         PIC 9(5) VALUE 0.
       01 WS-CAL-TOTAL-BONUS-PERCENT    PIC 9(3)V99 VALUE 0.00.
       01 WS-CAL-NO-BONUS-PERCENT       PIC 9(3)V99 VALUE 0.00.
       01 WS--CAL-NO-BONUS-PERCENT-VALUE
                                        PIC 999V99 VALUE 0.00.
       01 WS-TOTAL-EARNED               PIC 9(9)V99 VALUE 0.00.

       77 WS-HUNDRED                    PIC 999V99 VALUE 100.00.


       01 WS-CALCULATION-SALES          PIC 9(6).
       01 WS-CALCULATION-MAX            PIC 9(6).
       01 WS-CALCULATION-MIN            PIC 9(6).
       01 WS-CALCULATION-RATE           PIC 9(3)V9(2).
       01 WS-PERCENTAGE-AMMOUNT         PIC 9(3)V9(2).
       01 WS-CALCULATION-EARNED         PIC 9(6)V9(2).
       01 WS-CALCULATION-PAID           PIC 9(6)V9(2).
       01 WS-CALCULATION-BONUS          PIC 9(2)V9(2).
       01 WS-REMARK                     PIC X(20).

       77 MAX-SALES-THRESHOLD           PIC 9(6) VALUE 300000.
       77 BONUS-RATE                    PIC 9(5)V99 VALUE 15.25.
       77 WS-LINES-PER-PAGE             PIC 99 VALUE 5.
       77 WS-PAGE-COUNT                 PIC 99 VALUE 0.
       77 WS-LINE-COUNT                 PIC 99 VALUE 0.


        PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.

           WRITE OUTPUT-LINE FROM WS-HEADER-LINE
              AFTER ADVANCING 1 LINE.
           WRITE OUTPUT-LINE FROM WS-EMPTY-LINE
              AFTER ADVANCING 1 LINE.


           READ INPUT-FILE
           AT END
              MOVE "y" TO WS-EOF-FLAG.


           PERFORM 100-PROCESS-FILE UNTIL WS-EOF-FLAG = "y".
           PERFORM 150-PRINT-PAGE-TOTALS.
           PERFORM 200-WRITES.

           CLOSE INPUT-FILE OUTPUT-FILE.
           GOBACK.

       100-PROCESS-FILE.
           PERFORM 110-PRINT-HEADINGS.
           PERFORM 130-PROCESS-LINES
              VARYING WS-LINE-COUNT FROM 1 BY 1
              UNTIL(WS-LINE-COUNT > WS-LINES-PER-PAGE
              OR WS-EOF-FLAG = "y").
      *




       110-PRINT-HEADINGS.

           ADD 1 TO WS-PAGE-COUNT.
           MOVE SPACES TO OUTPUT-LINE.
           IF (WS-PAGE-COUNT > 1) THEN
              WRITE OUTPUT-LINE
                 AFTER ADVANCING PAGE
              WRITE OUTPUT-LINE
              WRITE OUTPUT-LINE FROM WS-TITLE-LINE
              MOVE SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              WRITE OUTPUT-LINE FROM WS-COLUMN-HEADINGS
              MOVE SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
           ELSE
              WRITE OUTPUT-LINE FROM WS-TITLE-LINE
              MOVE SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              WRITE OUTPUT-LINE FROM WS-COLUMN-HEADINGS
              MOVE SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
           END-IF.
       130-PROCESS-LINES.
           MOVE NOM TO WS-NUMBER
           MOVE NAME TO WS-NAME
           MOVE SALES TO WS-CALCULATION-SALES
           MOVE MIN-COMM TO WS-MIN
           MOVE MAX-COMM TO WS-CALCULATION-MAX
           MOVE COMM TO WS-CALCULATION-RATE


           ADD 1 TO WS-TOTAL-SALESPERSONS-VALUE

           DISPLAY "NOM=" WS-NUMBER.
           DISPLAY "Name=" WS-NAME.
           DISPLAY "SALES=" WS-CALCULATION-SALES.
           DISPLAY "MIN-COMM=" WS-MIN.
           DISPLAY "MAX-COMM=" WS-CALCULATION-MAX.
           DISPLAY "COMM=" WS-CALCULATION-RATE.



           IF WS-CALCULATION-SALES <= MAX-SALES-THRESHOLD
              ADD 1 TO WS-BONUS-MORE-MAX-VALUE
              COMPUTE WS-CALCULATION-EARNED
                 =(WS-CALCULATION-SALES
                 * WS-CALCULATION-RATE) / 100

           ELSE
              ADD 1 TO WS-BONUS-LESS-MAX-VALUE
              COMPUTE WS-CALCULATION-EARNED
                 =((WS-CALCULATION-SALES * WS-CALCULATION-RATE)
                 / 100) +
                 ((WS-CALCULATION-SALES - MAX-SALES-THRESHOLD)
                 *(BONUS-RATE) / 100)
           END-IF.

           IF WS-CALCULATION-SALES > MAX-SALES-THRESHOLD
              IF WS-CALCULATION-EARNED > WS-CALCULATION-MAX
                 COMPUTE WS-CALCULATION-PAID = WS-CALCULATION-MAX
              ELSE
                 COMPUTE WS-CALCULATION-PAID = WS-CALCULATION-EARNED
              END-IF
           ELSE
              IF WS-CALCULATION-EARNED < WS-CALCULATION-MIN
                 COMPUTE WS-CALCULATION-PAID = WS-CALCULATION-MIN
              ELSE
                 COMPUTE WS-CALCULATION-PAID = WS-CALCULATION-EARNED
              END-IF
           END-IF.

           COMPUTE WS-CALCULATION-PAID =
              FUNCTION MIN(WS-CALCULATION-EARNED, MAX-COMM).

           IF WS-CALCULATION-PAID = WS-CALCULATION-EARNED
              ADD 1 TO WS-TOTAL-CAL-EQUAL-EARNED
           END-IF.

           DIVIDE WS-TOTAL-CAL-EQUAL-EARNED
              BY WS-TOTAL-SALESPERSONS-VALUE
              GIVING WS-CAL-TOTAL-BONUS-PERCENT.
           COMPUTE WS-CAL-TOTAL-BONUS-PERCENT =
              WS-CAL-TOTAL-BONUS-PERCENT * 100.

           DIVIDE WS-TOTAL-BONUS BY WS-TOTAL-SALESPERSONS-VALUE
              GIVING WS-CAL-NO-BONUS-PERCENT.
           COMPUTE WS-CAL-NO-BONUS-PERCENT = WS-CAL-NO-BONUS-PERCENT
              * 100



           COMPUTE WS--CAL-NO-BONUS-PERCENT-VALUE =
              (WS-TOTAL-SALESPERSONS-VALUE - WS-TOTAL-BONUS)
              / WS-TOTAL-SALESPERSONS-VALUE
           COMPUTE WS--CAL-NO-BONUS-PERCENT-VALUE =
              WS--CAL-NO-BONUS-PERCENT-VALUE * 100

           ADD WS-CALCULATION-EARNED TO WS-TOTAL-EARNED
           ADD WS-CALCULATION-PAID TO WS-TOTAL-PAID
           MOVE WS-TOTAL-EARNED TO WS-EARNED-TOTAL-VALUE
           MOVE WS-TOTAL-PAID TO WS-PAID-TOTAL-VALUE
           MOVE WS-CAL-TOTAL-BONUS-PERCENT TO WS-TOTAL-PERCENT-VALUE
           MOVE WS--CAL-NO-BONUS-PERCENT-VALUE TO
              WS-NO-BONUS-PERCENT-VALUE
           MOVE WS-CAL-NO-BONUS-PERCENT TO WS-BONUS-PERCENT-VALUE
           MOVE WS-CALCULATION-SALES TO WS-SALES
           MOVE WS-CALCULATION-MAX TO WS-MAX
           MOVE WS-CALCULATION-RATE TO WS-RATE
           MOVE WS-CALCULATION-EARNED TO WS-EARNED
           MOVE WS-CALCULATION-PAID TO WS-PAID

           IF WS-CALCULATION-SALES > MAX-SALES-THRESHOLD
              ADD 1 TO WS-TOTAL-BONUS-VALUE
              MOVE "BONUS EARNED" TO WS-REMARK
           ELSE
              ADD 1 TO WS-TOTAL-NO-BONUS-VALUE
              MOVE "BONUS NOT EARNED" TO WS-REMARK

           END-IF.

           MOVE WS-REMARK TO WS-BONUS.


           WRITE OUTPUT-LINE FROM WS-DEATIL-LINE AFTER
              ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-EMPTY-LINE AFTER ADVANCING 1 LINE.


           READ INPUT-FILE
           AT END
              MOVE "y" TO WS-EOF-FLAG.
       150-PRINT-PAGE-TOTALS.
           WRITE OUTPUT-LINE FROM WS-GRAND-TOTALS
              AFTER ADVANCING 2 LINES.
       200-WRITES.

           WRITE OUTPUT-LINE FROM WS-BONUS-MORE-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-BONUS-LESS-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-EMPTY-LINE
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-TOTAL-BONUS-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-TOTAL-NO-BONUS-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-TOTAL-SALESPERSONS-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-EMPTY-LINE
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-TOTAL-PAID-EQUAL-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-TOTAL-PERCENT-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-EMPTY-LINE
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-BONUS-PERCENT-DETAIL
              AFTER ADVANCING 2 LINES.
           WRITE OUTPUT-LINE FROM WS-NO-BONUS-PERCENT-DETAIL
              AFTER ADVANCING 2 LINES.


      *
       END PROGRAM A3SCOMM.