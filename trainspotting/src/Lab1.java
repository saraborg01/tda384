import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import static java.util.Arrays.asList;
import TSim.*;

public class Lab1 {
	
	enum Sensors {
		CROSSING_W, CROSSING_S, CROSSING_E, CROSSING_N,
		EAST_E, EAST_S, EAST_W, 
		MID_E, MID_S, MID_W,
		WEST_W, WEST_E, WEST_S,
		SOUTH_W, SOUTH_E, SOUTH_S,
		NORTH_STATION_N, NORTH_STATION_S, 
		SOUTH_STATION_N, SOUTH_STATION_S
	}
	
	enum Switches {
		NORTH, EAST, WEST, SOUTH
	}
	
	enum Semaphores {
		N_STATION, CROSSING, EAST_TRACK,
		WEST_TRACK, MID_TRACK, S_STATION
	}
	
	Map<Sensors, List<Integer>> sensors   = new HashMap<Sensors, List<Integer>>();
	Map<Switches, List<Integer>> switches = new HashMap<Switches, List<Integer>>();
	Map<Semaphores, Semaphore> semaphores = new HashMap<Semaphores, Semaphore>();
	Map<List<Integer>, Sensors> sensorPos = new HashMap<List<Integer>, Sensors>();
	

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    
    sensors.put(Sensors.CROSSING_W, asList(6,5));
    sensors.put(Sensors.CROSSING_N, asList(8,6));
    sensors.put(Sensors.CROSSING_E, asList(10,7));
    sensors.put(Sensors.CROSSING_S, asList(9,8));
    
    sensors.put(Sensors.EAST_E, asList(19,7));
    sensors.put(Sensors.EAST_W, asList(16,7));
    sensors.put(Sensors.EAST_S, asList(16,8));
    
    sensors.put(Sensors.MID_E, asList(17,9));
    sensors.put(Sensors.MID_W, asList(14,9));
    sensors.put(Sensors.MID_S, asList(14,10));
    
    sensors.put(Sensors.WEST_E, asList(5,9));
    sensors.put(Sensors.WEST_W, asList(2,9));
    sensors.put(Sensors.WEST_S, asList(5,10));
    
    sensors.put(Sensors.SOUTH_E, asList(4,11));
    sensors.put(Sensors.SOUTH_W, asList(1,11));
    sensors.put(Sensors.SOUTH_S, asList(3,12));
    
    sensors.put(Sensors.NORTH_STATION_N, asList(16,0));
    sensors.put(Sensors.NORTH_STATION_S, asList(16,5));
    sensors.put(Sensors.SOUTH_STATION_N, asList(14,11));
    sensors.put(Sensors.SOUTH_STATION_S, asList(14,14));
    
    switches.put(Switches.NORTH, asList(17,7));
    switches.put(Switches.EAST, asList(15,9));
    switches.put(Switches.WEST, asList(4,9));
    switches.put(Switches.SOUTH, asList(3,11));
    
    semaphores.put(Semaphores.CROSSING, new Semaphore(1));
    semaphores.put(Semaphores.EAST_TRACK, new Semaphore(1));
    semaphores.put(Semaphores.WEST_TRACK, new Semaphore(1));
    semaphores.put(Semaphores.MID_TRACK, new Semaphore(1));
    semaphores.put(Semaphores.N_STATION, new Semaphore(0));
    semaphores.put(Semaphores.S_STATION, new Semaphore(0));
    
    for (Map.Entry<Sensors, List<Integer>> sens: sensors.entrySet()) {
    	sensorPos.put(sens.getValue(), sens.getKey());
    }
    
      
      Train train1 = new Train(1,speed1,tsi);
      Train train2 = new Train(2,speed2,tsi);
      
      
      
      train1.start();
      train2.start();
    

    
  }
  
  
  
  
  private void setClearance() {
	// TODO Auto-generated method stub
	
}

private void waitForClearance() {
	// TODO Auto-generated method stub
	
}


class Train extends Thread {
		
		private int id;
		private int speed;
		private TSimInterface tsi;
		private Sensors prevSensor;
		private Sensors currentSensor;
		
		public Train(int id, int speed, TSimInterface tsi) {
			this.id = id;
			this.speed = speed;
			this.tsi = tsi;
		}
		
		private void handleSensor(SensorEvent event) {
			Sensors name = sensorPos.get(asList(event.getXpos(), event.getYpos()));
			
			if (event.getStatus() == SensorEvent.ACTIVE) {
				System.out.println(name);
				switch (name) {
				// cases for crossing
					case CROSSING_W:
						if (prevSensor == Sensors.NORTH_STATION_N) {
							waitForClearance();
						} else
							setClearance();
						System.out.println("Crossing the crossing west");
					case CROSSING_N:
						if (prevSensor == Sensors.NORTH_STATION_S) {
							waitForClearance();
						} else
							setClearance();

						
				}
			}
	    }
		
		@Override
		public void run() {
			try {
				tsi.setSpeed(id,speed);
				System.out.println(id + " going");
				
				while(true) {
					SensorEvent event = tsi.getSensor(id);
					handleSensor(event);

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
  
  
  
  
}


