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
		EAST, MID, WEST, SOUTH
	}
	
	enum Semaphores {
		N_STATION, CROSSING, EAST_TRACK,
		WEST_TRACK, MID_TRACK, S_STATION
	}
	
	Map<Sensors, List<Integer>> sensors   = new HashMap<>();
	Map<Switches, List<Integer>> switches = new HashMap<>();
	Map<Semaphores, Semaphore> semaphores = new HashMap<>();
	Map<List<Integer>, Sensors> sensorPos = new HashMap<>();
	

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
	    
	    switches.put(Switches.EAST, asList(17,7));
	    switches.put(Switches.MID, asList(15,9));
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
    
      
	    Train train1 = new Train(1,speed1,tsi, Sensors.NORTH_STATION_N);
	    Train train2 = new Train(2,speed2,tsi, Sensors.SOUTH_STATION_N);
      
	    train1.start();
	    train2.start();
	    
    
    }


class Train extends Thread {
		
		private int id;
		private int speed;
		private TSimInterface tsi;
		private Sensors lastSensor;
		
		public Train(int id, int speed, TSimInterface tsi, Sensors lastSensor) {
			this.id = id;
			this.speed = speed;
			this.tsi = tsi;
			this.lastSensor = lastSensor;
		}
		
	    private void setClearance() {
	    	// TODO Auto-generated method stub
	    	
//	    	System.out.println("setting clearance");
		
	    }	

		private void waitForClearance() {
			// TODO Auto-generated method stub
			
//			System.out.println("waiting at clearance");
			
		}
		
		private void waitAtStation() {
			System.out.println("waiting at station");
			try {
				tsi.setSpeed(id, 0);
			} catch (CommandException e) {
				e.printStackTrace();
			} 
//			catch (InterruptedException e) {
//				e.printStackTrace();
//			}
			
			
		}
		
		private boolean semaphoreIsAvailable(Semaphores sem) {
//			System.out.println("checking availability" + sem);
			return false;
		}
		
		private void changeSwitch(Switches s, int direction) throws CommandException{
			List<Integer> pos = switches.get(s);
			tsi.setSwitch(pos.get(0), pos.get(1), direction);
//			System.out.println("changing switch" + s);
		}
		
		private void setDefaultSwitches() throws CommandException{
	    	List<Integer> eastPos = switches.get(Switches.EAST);
	    	List<Integer> midPos = switches.get(Switches.MID);
	    	tsi.setSwitch(eastPos.get(0), eastPos.get(1), tsi.SWITCH_RIGHT);
	    	tsi.setSwitch(midPos.get(0), midPos.get(1), tsi.SWITCH_RIGHT);
	    }
		
		private void handleSensor(SensorEvent event) throws CommandException, InterruptedException {
			Sensors name = sensorPos.get(asList(event.getXpos(), event.getYpos()));
			
			if (event.getStatus() == SensorEvent.ACTIVE) {
				System.out.println("New activated sensor: " + name);
				
				switch (name) {
				// cases for crossing
					case CROSSING_W:
						if (lastSensor == Sensors.NORTH_STATION_N) {
							waitForClearance();
							// claim crossing
						} else {
							setClearance(); // on crossing
						}
						break;
					case CROSSING_N:
						if (lastSensor == Sensors.NORTH_STATION_S) {
							waitForClearance();
							// claim crossing
						} else {
							setClearance(); // on crossing
						}
						break;
					case CROSSING_E:
						if (lastSensor == Sensors.EAST_W) {
							waitForClearance();
							// claim crossing
						} else {
							setClearance();
						} // on crossing
						break;
					case CROSSING_S: 
						if (lastSensor == Sensors.EAST_S) {
							waitForClearance();
							// claim crossing
						} else {
							setClearance(); // on crossing
						}
						break;

					// East-top junction
					case EAST_W:
						if (lastSensor == Sensors.CROSSING_E) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(); // but with switch
								// claim east track
							} else {
								changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT);
								// claim east track
								// maybe update speed
							}
						} else {
							setClearance(); // on east track
						}
						break;
					case EAST_S:
						if (lastSensor == Sensors.CROSSING_S) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(); // but with switch
								// claim east track
							} else {
								changeSwitch(Switches.EAST, tsi.SWITCH_LEFT);
								// claim east track
								// maybe update speed
							}
						} else {
							setClearance(); // on EAST track
							changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT); // kanske inte behövs
						}
						break;
					case EAST_E:
						if (lastSensor == Sensors.MID_E) {
							if (!semaphoreIsAvailable(Semaphores.N_STATION))
								changeSwitch(Switches.EAST, tsi.SWITCH_LEFT);
								// maybe update speed
							else {
								// åka framåt
								// claim north station
								// maybe update switch
							}
						} else if (lastSensor == Sensors.EAST_W){
							setClearance(); // on north station
						} else {
							changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT);
						}
						break;
						
						
					// Mid junction
					case MID_E:
						if (lastSensor == Sensors.EAST_E) {
							if (!semaphoreIsAvailable(Semaphores.MID_TRACK)) {
								changeSwitch(Switches.MID, tsi.SWITCH_LEFT);
								
							} else {
								// claim mid track
								//åk fram
							}	
						} else if (lastSensor == Sensors.MID_W) {
							setClearance(); // on mid track
						}  else {
							changeSwitch(Switches.MID, tsi.SWITCH_RIGHT);
						}
						break;
					case MID_W:
						if (lastSensor == Sensors.WEST_E) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)){
								waitForClearance(); // but with switch
								// claim east track
							} else {
								// åka fram
								// claim east track
							}
						} else {
							setClearance(); // east track
						}
						break;
					case MID_S:
						if (lastSensor == Sensors.WEST_S) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(); // but with switch
								// claim east track
							} else {
								changeSwitch(Switches.MID, tsi.SWITCH_LEFT);
								// claim east track
							}
						} else {
							setClearance(); // on east track
							changeSwitch(Switches.MID, tsi.SWITCH_RIGHT);
						}
						break;
						
					// West junction
					case WEST_W:
						if (lastSensor == Sensors.SOUTH_W) {
							if (!semaphoreIsAvailable(Semaphores.MID_TRACK)) {
								changeSwitch(Switches.WEST, tsi.SWITCH_RIGHT);
							} else {
								// claim mid track
								// åk fram
							}
						} else if (lastSensor == Sensors.WEST_E) {
							setClearance(); // on mid track
						} else {
							changeSwitch(Switches.WEST, tsi.SWITCH_LEFT);
						}
						break;
					case WEST_E:
						if (lastSensor == Sensors.MID_W) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(); 
								// claim west track
							} else {
								// claim west track
								// åk fram
							}
						} else {
							setClearance(); // on west track
						}
						break;
					case WEST_S:
						if (lastSensor == Sensors.MID_S) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(); // but with switch
								// claim west track
							} else {
								changeSwitch(Switches.WEST, tsi.SWITCH_RIGHT);
								// åk fram
								// claim west track
							}
						} else {
							setClearance(); // on west track
							changeSwitch(Switches.WEST, tsi.SWITCH_LEFT);
						}
						break;
						
					// South junction
					case SOUTH_W:
						if (lastSensor == Sensors.WEST_W) {
							if (!semaphoreIsAvailable(Semaphores.S_STATION)) {
								changeSwitch(Switches.SOUTH, tsi.SWITCH_RIGHT);
							} else {
								// claim south station
								// åka fram
							}
						} else if (lastSensor == Sensors.SOUTH_E){
							setClearance(); // on south station
						} else {
						    changeSwitch(Switches.SOUTH, tsi.SWITCH_LEFT);
						}
						break;
					case SOUTH_E:
						if (lastSensor == Sensors.SOUTH_STATION_N) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance();
								// claim west track
							} else {
								// claim west track
							}
						} else {
							setClearance(); // on west track
						}
						break;
					case SOUTH_S:
						if (lastSensor == Sensors.SOUTH_STATION_S) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(); // but with switch
								// claim west track
							} else {
								changeSwitch(Switches.SOUTH, tsi.SWITCH_RIGHT);
								// claim west track
							}
						} else {
							changeSwitch(Switches.SOUTH, tsi.SWITCH_LEFT);
							setClearance(); // on west track
						}
						break;
					
					// Stations
					case NORTH_STATION_N:
						if (lastSensor == Sensors.CROSSING_W) {
							System.out.println("north station n");
							waitAtStation();
						}
						break;
					case SOUTH_STATION_N:
						if (lastSensor == Sensors.SOUTH_E) {
							System.out.println("south station n");
							waitAtStation();
						}
						break;
					case NORTH_STATION_S:
						if (lastSensor == Sensors.CROSSING_N) {
							System.out.println("north station s");
							waitAtStation();
						}
						break;
					case SOUTH_STATION_S:
						if (lastSensor == Sensors.SOUTH_S) {
							System.out.println("south station s");
							waitAtStation();
						}
						break;
				}
				
				System.out.println("new last sensor " + name);
				
				lastSensor = name;
			}
	    }
		
		@Override
		public void run() {
			try {
				setDefaultSwitches();
				
				tsi.setSpeed(id,speed);
				
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


