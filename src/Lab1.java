import TSim.*;

public class Lab1 {

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1,speed1);
      tsi.setSpeed(2,speed2);
      
      
      // create threads
      Train train1 = new Train(1,speed1);
      Train train2 = new Train(2,speed2);
      // start threads
      
      Thread t1 = new Thread(train1);
      Thread t2 = new Thread(train2);
      
      t1.start();
      t2.start();
      
      tsi.setSwitch(15, 9, 2);
      
      // tsi.getSensor(2);
     
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
    
  }
}

class Train implements Runnable {
	
	private int id;
	private int speed;
	private TSimInterface tsi = TSimInterface.getInstance();
	
	public Train(int id, int speed) {
		this.id = id;
		this.speed = speed;
	}
	
	@Override
	public void run() {
		try {
			System.out.println(id + " going");
			while(true) {
				SensorEvent event = tsi.getSensor(id);
			}
		}
		catch (CommandException e) {
			e.getStackTrace();
			System.exit(1);
		}
		catch (InterruptedException e) {
			e.getStackTrace();
			System.exit(1);
		}
		
		// TODO Auto-generated method stub
		
	}
	
}
