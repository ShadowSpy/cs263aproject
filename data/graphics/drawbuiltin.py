
#from __future__ import division

#More actions

#avoid, enter-from top/bottom/right/left




from graphics import *
import time
import math
win = GraphWin('Back and Forth', 800, 600)
class rectobj:
    
    def __init__(self,upperleft,lowerright,color):
        self.upperleft = upperleft
        self.lowerright = lowerright
        
        #the graphic library only requires a retangle has upperleft and lowerright
        #but we calculates the lowerleft and upperright here for further use 
        self.lowerleft = Point(upperleft.x,lowerright.y)
        self.upperright = Point(lowerright.x,upperleft.y)
        
        #color
        self.color = color
        
        #automatically calculates the centers of the objects and the sides
        #for the person to move to
        
        self.leftcenter = Point(upperleft.x,(upperleft.y+self.lowerleft.y)/2)
        self.rightcenter = Point(self.upperright.x,(self.upperright.y+lowerright.y)/2)
        self.uppercenter = Point((upperleft.x+self.upperright.x)/2,upperleft.y)
        self.lowercenter = Point((self.lowerleft.x+lowerright.x)/2,self.lowerleft.y)
        
        self.center = Point((self.leftcenter.x+self.rightcenter.x)/2,(self.uppercenter.y+self.lowercenter.y)/2)



objectlist = [rectobj(Point(30,50),Point(50,80),"yellow"),rectobj(Point(400,220),Point(430,250),"blue"),rectobj(Point(180,220),Point(200,240),"red"),rectobj(Point(580,580),Point(620,600),"green")]


#moving a to b

def moveto(a,b,avoid=False):
    ax = a.getCenter().x
    ay = a.getCenter().y
    
    bx = b.center.x
    by = b.center.y
    
    movex = 0
    movey = 0
    
    ratio = (float(by-ay)/float(bx-ax)) * 1.0
    if ax > bx:
        movex = -2.0
    else:
        movex = 2.0
    
    if ay > by:
        movey = -ratio*2.0
    else:
        movey = ratio*2.0
    print("a is :"+str(ax)+" , "+str(ay))
    print("b is :"+str(bx)+" , "+str(by))
    
    print("ratio is :"+str(ratio)+" , ")
    
    print("move is :"+str(movex)+" , "+str(movey)+"\n\n")
    
    a.move(movex*1.1,movey*1.1)
    
def enterfromleft(a,b):
    moveto(a,b.leftcenter)

def enterfromright(a,b):
    moveto(a,b.rightcenter)

def enterfromupper(a,b):
    moveto(a,b.uppercenter)

def enterfromlower(a,b):
    moveto(a,b.lowercenter)

# a global variable determines if it has enter
# if so, go out from the other side,
# if not, try to get in
# this function can't be written outside a loop environment
# and doesn't need to be a named function inside a loop environment
# just representing some complex behaviors 
#def enterleftexitright(a,b):
    

def near(a,b):
    
    ax = a.getCenter().x
    ay = a.getCenter().y
    
    bx = b.center.x
    by = b.center.y
    
    
    distance = math.sqrt((bx-ax)*(bx-ax)+(by-ay)*(by-ay))
    
    return (distance < 4)

def drawlist(list):
    for a in list:
        rect = Rectangle(a.upperleft, a.lowerright)
        rect.setFill(a.color)
        rect.draw(win)
        #rect.setFill("yellow")
def main():

    drawlist(objectlist)

    m = rectobj(Point(0,0),Point(20,20),"yellow")
    m1 = Rectangle(m.upperleft, m.lowerright)
    m1.setFill(m.color)
    m1.draw(win)
    
    i = 0
    while True:
        while i < len(objectlist):
            if (near(m1,objectlist[i])):
                i+=1
            else:
                moveto(m1,objectlist[i])
            time.sleep(.05)
        i = 0
    win.Close(win.getWidth()/2, 20)

main()

