*>****************************************************************
*> Author: OS
*> Date: 181012
*> Purpose: Dijkstra's Algorithm
*> Tectonics: cobc
*>****************************************************************

*> Adjacent matrix
*>  |A|B|C|D|E|F|
*> -+-+-+-+-+-+-+
*> A|0|2|4|0|0|0|
*> -+-+-+-+-+-+-+
*> B|2|0|1|4|2|0|
*> -+-+-+-+-+-+-+
*> C|4|1|0|0|3|0|
*> -+-+-+-+-+-+-+
*> D|0|4|0|0|3|2|
*> -+-+-+-+-+-+-+
*> E|0|2|3|3|0|2|
*> -+-+-+-+-+-+-+
*> F|0|0|0|2|2|0|
*> -+-+-+-+-+-+-+
IDENTIFICATION DIVISION.
PROGRAM-ID. dijkstras_algorithm.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.

01 networkAdjacentTable.
    02 networkAdjacentTableHOcr occurs 6.
        03 networkHNode pic x(1).
        03 networkHWeightVal pic 9(6).
        03 networkHNodeVisited pic 9(1).
            88 hNodeIsVisited value 1, false 0.
        03 networkAdjacentTableVOcr occurs 6.
            04 networkVNode pic x(1).
            04 networkVWeightVal pic 9(6).
            04 networkDistanceVal pic 9(6).
            04 networkNodeVisited pic 9(1) value 0.
                88 nodeIsVisited value 1, false 0.

01 trackingPathTable.
    02 trackingPathHeadTableOcr occurs 6.
        03 trackingPathHead pic x(1).
        03 trackingPathTableGrp.
            04 trackingPathTableOcr occurs 5.
                05 trackingPathNodeVal pic x(1).

01 pathBuilderTable.
    02 pathBuilderOcr occurs 5.
        03 pathBuilderVal pic x(1).

01 currentNodeDataArea.
    02 currentNodeVal pic x(1).
    02 currentWieghtVal pic 9(6).
    02 currentNodeDistanceVal pic 9(6).
    02 currentXValue pic 9(6).
    02 currentYValue pic 9(6).
01 lowestNodeDataArea.
    02 lowestNodeVal pic x(1).
    02 lowestWieghtVal pic 9(6).
    02 lowestNodeDistanceVal pic 9(6).
    02 lowestXValue pic 9(6).
    02 lowestYValue pic 9(6).
01 previousNodeDataArea.
    02 previousNodeVal pic x(1).
    02 previousWeightVal pic 9(6).
    02 previousNodeDistanceVal pic 9(6).
    02 previousXValue pic 9(6).
    02 previousYValue pic 9(6).

01 x pic 9(6).
01 y pic 9(6).
01 z pic 9(6).
01 p pic 9(6).
01 o pic 9(6).
01 i pic 9(6).
01 iteration pic 9(6).
01 nodeMarkAsVisited pic 9(1).
    88 visited value 1, false 0.

01 skipNode pic 9(1).
    88 skip value 1, false 0.

01 debugLogger pic x(2000).
01 displayToLogFile pic 9(1).
    88 dsiplayNodesAfterVisitFalse value 1, false 0.

01 weightValueFromHNode pic 9(6).
01 valueToDisplay pic 9(6).
01 destinationWeightNode pic 9(6).
01 validateSumToDestNodeWeight pic 9(6).
01 showThisNode pic 9(1).
    88 showNode value 1, false 0.

PROCEDURE DIVISION.


    perform createNetwork
    move 0 to iteration
    perform dAlgorithm
    perform terminateProgram.


*>---------------------------
createNetwork section.
*>---------------------------
    move "A" to networkHNode(1)
    move 0 to networkHWeightVal(1)
    move "A" to networkVNode(1,1)
    move 0 to networkVWeightVal(1,1)
    move 0 to networkDistanceVal(1,1)
    set nodeIsVisited(1,1) to true

    move "B" to networkVNode(1,2)
    move high-value to networkVWeightVal(1,2)
    move 2 to networkDistanceVal(1,2)
    set nodeIsVisited(1,2) to false

    move "C" to networkVNode(1,3)
    move high-value to networkVWeightVal(1,3)
    move 4 to networkDistanceVal(1,3)
    set nodeIsVisited(1,3) to false

    move "D" to networkVNode(1,4)
    move high-value to networkVWeightVal(1,4)
    move 0 to networkDistanceVal(1,4)
    set nodeIsVisited(1,4) to false

    move "E" to networkVNode(1,5)
    move high-value to networkVWeightVal(1,5)
    move 0 to networkDistanceVal(1,5)
    set nodeIsVisited(1,5) to false

    move "F" to networkVNode(1,6)
    move high-value to networkVWeightVal(1,6)
    move 0 to networkDistanceVal(1,6)
    set nodeIsVisited(1,6) to false

    *>*>B
    move "B" to networkHNode(2)
    move high-value to networkHWeightVal(2)

    move "A" to networkVNode(2,1)
    move 0 to networkVWeightVal(2,1)
    move 2 to networkDistanceVal(2,1)
    set nodeIsVisited(2,1) to true

    move "B" to networkVNode(2,2)
    move high-value to networkVWeightVal(2,2)
    move 0 to networkDistanceVal(2,2)
    set nodeIsVisited(2,2) to false

    move "C" to networkVNode(2,3)
    move high-value to networkVWeightVal(2,3)
    move 1 to networkDistanceVal(2,3)
    set nodeIsVisited(2,3) to false

    move "D" to networkVNode(2,4)
    move high-value to networkVWeightVal(2,4)
    move 4 to networkDistanceVal(2,4)
    set nodeIsVisited(2,4) to false

    move "E" to networkVNode(2,5)
    move high-value to networkVWeightVal(2,5)
    move 2 to networkDistanceVal(2,5)
    set nodeIsVisited(2,5) to false

    move "F" to networkVNode(2,6)
    move high-value to networkVWeightVal(2,6)
    move 0 to networkDistanceVal(2,6)
    set nodeIsVisited(2,6) to false

    *>*> C
    move "C" to networkHNode(3)
    move high-value to networkHWeightVal(3)

    move "A" to networkVNode(3,1)
    move 0 to networkVWeightVal(3,1)
    move 4 to networkDistanceVal(3,1)
    set nodeIsVisited(3,1) to true

    move "B" to networkVNode(3,2)
    move high-value to networkVWeightVal(3,2)
    move 1 to networkDistanceVal(3,2)
    set nodeIsVisited(3,2) to false

    move "C" to networkVNode(3,3)
    move high-value to networkVWeightVal(3,3)
    move 0 to networkDistanceVal(3,3)
    set nodeIsVisited(3,3) to false

    move "D" to networkVNode(3,4)
    move high-value to networkVWeightVal(3,4)
    move 0 to networkDistanceVal(3,4) *> obsobs change back to 0
    set nodeIsVisited(3,4) to false

    move "E" to networkVNode(3,5)
    move high-value to networkVWeightVal(3,5)
    move 3 to networkDistanceVal(3,5)
    set nodeIsVisited(3,5) to false

    move "F" to networkVNode(3,6)
    move high-value to networkVWeightVal(3,6)
    move 0 to networkDistanceVal(3,6)
    set nodeIsVisited(3,6) to false

    *>*> D
    move "D" to networkHNode(4)
    move high-value to networkHWeightVal(4)

    move "A" to networkVNode(4,1)
    move 0 to networkVWeightVal(4,1)
    move 0 to networkDistanceVal(4,1)
    set nodeIsVisited(4,1) to true

    move "B" to networkVNode(4,2)
    move high-value to networkVWeightVal(4,2)
    move 4 to networkDistanceVal(4,2)
    set nodeIsVisited(4,2) to false

    move "C" to networkVNode(4,3)
    move high-value to networkVWeightVal(4,3)
    move 0 to networkDistanceVal(4,3)
    set nodeIsVisited(4,3) to false

    move "D" to networkVNode(4,4)
    move high-value to networkVWeightVal(4,4)
    move 0 to networkDistanceVal(4,4)
    set nodeIsVisited(4,4) to false

    move "E" to networkVNode(4,5)
    move high-value to networkVWeightVal(4,5)
    move 3 to networkDistanceVal(4,5)
    set nodeIsVisited(4,5) to false

    move "F" to networkVNode(4,6)
    move high-value to networkVWeightVal(4,6)
    move 2 to networkDistanceVal(4,6)
    set nodeIsVisited(4,6) to false

    *>*> E
    move "E" to networkHNode(5)
    move high-value to networkHWeightVal(5)

    move "A" to networkVNode(5,1)
    move 0 to networkVWeightVal(5,1)
    move 0 to networkDistanceVal(5,1)
    set nodeIsVisited(5,1) to true

    move "B" to networkVNode(5,2)
    move high-value to networkVWeightVal(5,2)
    move 2 to networkDistanceVal(5,2)
    set nodeIsVisited(5,2) to false

    move "C" to networkVNode(5,3)
    move high-value to networkVWeightVal(5,3)
    move 3 to networkDistanceVal(5,3)
    set nodeIsVisited(5,3) to false

    move "D" to networkVNode(5,4)
    move high-value to networkVWeightVal(5,4)
    move 3 to networkDistanceVal(5,4)
    set nodeIsVisited(5,4) to false

    move "E" to networkVNode(5,5)
    move high-value to networkVWeightVal(5,5)
    move 0 to networkDistanceVal(5,5)
    set nodeIsVisited(5,5) to false

    move "F" to networkVNode(5,6)
    move high-value to networkVWeightVal(5,6)
    move 2 to networkDistanceVal(5,6)
    set nodeIsVisited(5,6) to false

    *>*> F
    move "F" to networkHNode(6)
    move high-value to networkHWeightVal(6)

    move "A" to networkVNode(6,1)
    move 0 to networkVWeightVal(6,1)
    move 0 to networkDistanceVal(6,1)
    set nodeIsVisited(6,1) to true

    move "B" to networkVNode(6,2)
    move high-value to networkVWeightVal(6,2)
    move 0 to networkDistanceVal(6,2)
    set nodeIsVisited(6,2) to false

    move "C" to networkVNode(6,3)
    move high-value to networkVWeightVal(6,3)
    move 0 to networkDistanceVal(6,3)
    set nodeIsVisited(6,3) to false

    move "D" to networkVNode(6,4)
    move high-value to networkVWeightVal(6,4)
    move 2 to networkDistanceVal(6,4)
    set nodeIsVisited(6,4) to false

    move "E" to networkVNode(6,5)
    move high-value to networkVWeightVal(6,5)
    move 2 to networkDistanceVal(6,5)
    set nodeIsVisited(6,5) to false

    move "F" to networkVNode(6,6)
    move high-value to networkVWeightVal(6,6)
    move 0 to networkDistanceVal(6,6)
    set nodeIsVisited(6,6) to false

exit section.

*>---------------------------
dAlgorithm section.
*>---------------------------
    perform varying x from 1 by 1 until x > 6
        set visited to false

        *> set path of initial node to itself e.g A->A
        if x = 1
            perform populatePathViewer
        end-if

        perform until visited
            perform getNodeWithLowestDistance
            if lowestXValue = 0
            or lowestYValue = 0
                exit perform
            end-if
            if networkVWeightVal(lowestXValue,lowestYValue) = high-value
                move 0 to networkVWeightVal(lowestXValue,lowestYValue)
            end-if
            move networkVWeightVal(lowestXValue,lowestYValue) to valueToDisplay
            perform getWeightValueOfNode
            compute networkVWeightVal(lowestXValue,lowestYValue) = weightValueFromHNode + lowestNodeDistanceVal
            set nodeIsVisited(lowestXValue, lowestYValue) to true
            perform checkConnectedNodesAreVisited
            perform updateNodeWithWeightValue
       end-perform
       set hNodeIsVisited(x) to true
       set visited to false
    end-perform

    perform varying p from 1 by 1 until p > 6
        display trackingPathHead(p) " -> " trackingPathTableGrp(p)
    end-perform
exit section.

*>------------------------------------
checkConnectedNodesAreVisited section.
*>------------------------------------
    set visited to false
    set dsiplayNodesAfterVisitFalse to false
    perform varying y from 1 by 1 until y > 6
        if networkDistanceVal(x,y) not = 0
            if nodeIsVisited(x,y)
                if dsiplayNodesAfterVisitFalse
                    set dsiplayNodesAfterVisitFalse to false
                    continue
                else
                    set visited to true
                end-if
            else
                set visited to false
                set dsiplayNodesAfterVisitFalse to true
            end-if
        end-if
    end-perform
exit section.

*>----------------------------
getWeightValueOfNode section.
*>----------------------------
    initialize weightValueFromHNode
    if networkHWeightVal(x) = high-value
        move 0 to weightValueFromHNode
    else
        move networkHWeightVal(x) to weightValueFromHNode
    end-if
exit section.

*>---------------------------------
updateNodeWithWeightValue section.
*>---------------------------------
    initialize destinationWeightNode
    perform varying y from 1 by 1 until y > 6
        if networkHNode(y) = networkVNode(lowestXValue, lowestYValue)
            if networkHWeightVal(y) >= networkVWeightVal(lowestXValue, lowestYValue)
                perform updatePathViewer
                move networkVWeightVal(lowestXValue, lowestYValue) to networkHWeightVal(y)
                move networkHWeightVal(y) to destinationWeightNode
                exit perform
            else
                move networkHWeightVal(y) to destinationWeightNode
            end-if
        end-if
    end-perform
exit section.

*>-------------------------
populatePathViewer section.
*>-------------------------
    perform varying p from 1 by 1 until p > 6
        move networkHNode(p) to trackingPathHead(p)
        if p = 1
            move networkHNode(p) to trackingPathNodeVal(p,1)
        end-if
    end-perform
exit section.

*>------------------------
updatePathViewer section.
*>------------------------
    initialize pathBuilderTable
    move 0 to i
    perform varying p from 1 by 1 until p > 6
        if trackingPathHead(p) = networkHNode(x)

            perform varying o from 1 by 1 until o > 5
                if trackingPathNodeVal(p,o) not = space
                    add 1 to i
                    move trackingPathNodeVal(p,o) to pathBuilderVal(i)
                end-if
            end-perform
        end-if
    end-perform

    perform varying p from 1 by 1 until p > 6
        if trackingPathHead(p) = networkVNode(lowestXValue,lowestYValue)
            perform varying o from 1 by 1 until o > 5
                if pathBuilderVal(o) = space
                    move networkVNode(lowestXValue,lowestYValue) to trackingPathNodeVal(p,o)
                    if o < 5
                        compute iteration = 5 - o
                        move space to trackingPathNodeVal(p,o + iteration)
                    end-if
                    exit perform
                else
                    move pathBuilderVal(o) to trackingPathNodeVal(p,o)
                end-if
            end-perform
        end-if
    end-perform

exit section.

*>----------------------------------
*>The section is used to get to the node with the shortest distance
getNodeWithLowestDistance section.
*>----------------------------------
    initialize currentNodeDataArea
               previousNodeDataArea
               lowestNodeDataArea
    perform varying y from 1 by 1 until y > 6
        perform checkVisitingNodeIsVisited
        if networkDistanceVal(x,y) > 0
        and not nodeIsVisited(x,y)
        and not skip
            move networkVNode(x,y) to currentNodeVal
            move networkDistanceVal(x,y) to currentNodeDistanceVal
            move x to currentXValue
            move y to currentYValue
            if y > 1
                if previousNodeDistanceVal < currentNodeDistanceVal
                and (previousNodeDistanceVal not = 0)
                    move previousNodeDataArea to lowestNodeDataArea
                else
                    if currentNodeDistanceVal < lowestNodeDistanceVal
                    or (lowestNodeDistanceVal = 0)
                        move currentNodeDataArea to lowestNodeDataArea
                    end-if
                end-if
            end-if
            move networkVNode(x,y) to previousNodeVal
            move networkDistanceVal(x,y) to previousNodeDistanceVal
            move x to previousXValue
            move y to previousYValue
        end-if
    end-perform
exit section.

*>----------------------------------
*> The section checks if the node has been marked as visited
checkVisitingNodeIsVisited section.
*>----------------------------------
    set skip to false
    perform varying z from 1 by 1 until z > 6
        if networkVNode(x,y) = networkHNode(z)
            if hNodeIsVisited(z)
                set skip to true
                set nodeIsVisited(x,y) to true
                exit perform
            end-if
        end-if
    end-perform
exit section.


*>---------------------------
terminateProgram section.
*>---------------------------
    stop run
exit section.
