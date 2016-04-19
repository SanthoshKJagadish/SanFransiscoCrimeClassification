import java.awt.*;

class CrimePoint {
  Point p;
  String weekday;
  int category;
  String district;
  
  CrimePoint(Point p, String wd, int cat, String dist) {
    this.p = p;
    this.weekday = wd;
    this.category = cat;
    this.district = dist;
  }
  
}

Table data;
PImage bg;
PImage district;
PGraphics buffer;
boolean waiting;
ArrayList<CrimePoint> list = new ArrayList<CrimePoint>();

int mode = 0;
int skip = 20;
int dot = 9;

void setup () {
  
  try {
    data = loadTable("train.csv", "header");
    bg = loadImage("map.png");
    district = loadImage("district.png");
    buffer = createGraphics(width, height);
  } catch (NullPointerException e) {
    exit();
  }
  
  size(1095, 993);
  frameRate(200);
  ellipseMode(CENTER);
  rectMode(CENTER);
  colorMode(HSB, 360, 100, 100);
  smooth();
  noLoop();
    
  for(TableRow row : data.rows()) {
    float lat = row.getFloat("Y");
    float longi = row.getFloat("X");
    int c = row.getInt("CategoryMap");
    String wd = row.getString("DayOfWeek");
    String pd = row.getString("PdDistrict");
    
    list.add(new CrimePoint(toXY(lat, longi), wd, c, pd));
  }
 
  background(bg);
}

void draw() {
  if(!mousePressed)
    return;
  showAll(mode);
}

//double toLL(double x, double y) {
//  ;
//}

Point toXY(float lat, float longi) {
 Point p = new Point();
 p.setLocation(763274 + 6229.38*longi, 293311 + -7752.36*lat  );
 return p;
}

void makeUI() {
  fill(0, 0, 100, 100);
  stroke(0, 0, 0);
  rect(width/2, 40, 700, 80);
  rect(240 , 110, 70, 60);
  rect(310 , 110, 70, 60);
  rect(380 , 110, 70, 60);
  rect(450 , 110, 70, 60);
  rect(520 , 110, 70, 60);
  rect(590 , 110, 70, 60);
  rect(660 , 110, 70, 60);
  rect(730 , 110, 70, 60);
  fill(0, 0, 0);
  text("Monday", 210, 110);
  text("Tuesday", 280, 110);
  text("Wednesday", 350, 110);
  text("Thursday", 420, 110);
  text("Friday", 490, 110);
  text("Saturday", 560, 110);
  text("Sunday", 630, 110);
  text("All week", 700, 110);
  
  stroke(110,100,100);
  fill(100, 100, 100);
  
  switch(mode) {
    case 0: rect(730, 115, 50, 4); 
            break;
    case 1: rect(240, 115, 50, 4);
            break;
    case 2: rect(310, 115, 50, 4);
            break;
    case 3: rect(380, 115, 50, 4);
            break;
    case 4: rect(450, 115, 50, 4);
            break;
    case 5: rect(520, 115, 50, 4);
            break;
    case 6: rect(590, 115, 50, 4);
            break; 
    case 7: rect(660, 115, 50, 4);
            break;
  }
  
  stroke(0, 0, 0);
  for(int i = 1; i <= 39; i++) {
    fill(i*9, 100, 100, 125);
    ellipse(i*16 + 197.5, 65, 12.0, 12.0);
    fill(0, 0, 0);
    text(i, i*16 + 197.5, 45);
  }
  text("CATEGORY", 16 + 197.5, 20);
}

void showAll(int mode) {
  makeUI();
  buffer.beginDraw();
  buffer.colorMode(HSB, 360, 100, 100);
  switch(mode) {
    case 0:
      println("~ALL WEEK~");
      for(int i = 0; i < list.size(); i+=skip) {
          fill(list.get(i).category*9, 100, 100, 100); 
          ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot); 
      }  image(district, 0, 0); break;
    case 1: 
      println("~MONDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Monday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 2: 
      println("~TUESDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Tuesday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 3: 
      println("~WEDNESDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Wednesday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 4: 
      println("~THURSDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Thursday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 5: 
      println("~FRIDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Friday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 6: 
      println("~SATURDAY~");
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Saturday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
    case 7: 
    println("~SUNDAY~" + mode);
      for(int i = 0; i < list.size(); i+=skip) {
        if(list.get(i).weekday.equals("Sunday")) {
          buffer.fill(list.get(i).category*9, 100, 100, 100); 
          buffer.ellipse(list.get(i).p.x, list.get(i).p.y, dot, dot);
        }
      } break;
  } 
  
  buffer.endDraw();
  image(buffer, 0, 0);
  buffer.clear();
  println("done with buffer! waiting");
}

void mousePressed() {
  clear();
  background(bg);
  mode++;
  if(mode > 7)
    mode = 0;
  redraw();
}