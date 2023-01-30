import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.lang.Math.*;

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
	    
	    sensors.put(Sensors.CROSSING_W, asList(6,6));
	    sensors.put(Sensors.CROSSING_N, asList(8,5));
	    sensors.put(Sensors.CROSSING_E, asList(10,7));
	    sensors.put(Sensors.CROSSING_S, asList(10,8));
	    
	    sensors.put(Sensors.EAST_E, asList(19,8));
	    sensors.put(Sensors.EAST_W, asList(15,7));
	    sensors.put(Sensors.EAST_S, asList(15,8));
	    
	    sensors.put(Sensors.MID_E, asList(17,9));
	    sensors.put(Sensors.MID_W, asList(13,9));
	    sensors.put(Sensors.MID_S, asList(13,10));
	    
	    sensors.put(Sensors.WEST_E, asList(6,9));
	    sensors.put(Sensors.WEST_W, asList(1,9));
	    sensors.put(Sensors.WEST_S, asList(6,10));
	    
	    sensors.put(Sensors.SOUTH_E, asList(5,11));
	    sensors.put(Sensors.SOUTH_W, asList(1,10));
	    sensors.put(Sensors.SOUTH_S, asList(4,13));
	    
	    sensors.put(Sensors.NORTH_STATION_N, asList(15,3));
	    sensors.put(Sensors.NORTH_STATION_S, asList(15,5));
	    sensors.put(Sensors.SOUTH_STATION_N, asList(15,11));
	    sensors.put(Sensors.SOUTH_STATION_S, asList(15,13));
	    
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
		
		private static int MAX_SPEED = 15;
		
		public Train(int id, int speed, TSimInterface tsi, Sensors lastSensor) {
			this.id = id;
			this.tsi = tsi;
			this.lastSensor = lastSensor;
			this.speed = getMaxSpeed(Math.abs(speed));
		}
		
		private int getMaxSpeed(int speed) {
			
			if (speed > MAX_SPEED) {
				return MAX_SPEED;
			} else {
				return speed;
			}
		}
		
	    private void setClearance(Semaphores semName) {
	    	Semaphore sem = semaphores.get(semName);
	    	if (sem.availablePermits() == 0 ) {
	    		sem.release();
			}
	    }	
	    
	    private void claim(Semaphores semName) throws InterruptedException{
	    	Semaphore sem = semaphores.get(semName);
	    	sem.acquire();

//	    	if (semName != Semaphores.CROSSING) {
//				lastSemaphore = currentSemaphore;
//				currentSemaphore = semName;
//			}
	    }

		private void waitForClearance(Semaphores semName) throws CommandException, InterruptedException{
		
			tsi.setSpeed(id, 0);
			claim(semName);
			
			tsi.setSpeed(id, speed);
			
		}
		
		private void waitForClearance(Semaphores semName, Switches s, int sDir) throws CommandException, InterruptedException{
			tsi.setSpeed(id, 0);
			claim(semName);
			changeSwitch(s, sDir);
			
			tsi.setSpeed(id, speed);
		}
		
		private void switchDirection() {
			this.speed *= -1;
		}
		
		private void waitAtStation() {
			System.out.println("waiting at station");
			try {
				tsi.setSpeed(id, 0);
				sleep(1000 + 2*Math.abs(speed));
				switchDirection();
				tsi.setSpeed(id, speed);
			} catch (CommandException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			
			
		}
		
		private boolean semaphoreIsAvailable(Semaphores semName) {
			Semaphore sem = semaphores.get(semName);
			// boolean notBusy = sem.tryAcquire();
			return sem.availablePermits() > 0;
		}
		
		private void changeSwitch(Switches s, int direction) throws CommandException{
			List<Integer> pos = switches.get(s);
			tsi.setSwitch(pos.get(0), pos.get(1), direction);
		}
		
		private void setDefaultSwitches() throws CommandException{
	    	List<Integer> eastPos = switches.get(Switches.EAST);
	    	List<Integer> midPos = switches.get(Switches.MID);
	    	tsi.setSwitch(eastPos.get(0), eastPos.get(1), tsi.SWITCH_RIGHT);
	    	tsi.setSwitch(midPos.get(0), midPos.get(1), tsi.SWITCH_RIGHT);
	    }
		
		private void handleSensor(SensorEvent event) throws CommandException, InterruptedException {
			Sensors name = sensorPos.get(asList(event.getXpos(), event.getYpos()));
			System.out.println(semaphores.get(Semaphores.WEST_TRACK).availablePermits());
			
			if (event.getStatus() == SensorEvent.ACTIVE) {
				if (id == 2) {
					System.out.println("NEW sensor: " + name);
					System.out.println("LAST sensor: " + lastSensor);
				}
				
				switch (name) {
				// cases for crossing
					case CROSSING_W:
						if (lastSensor == Sensors.NORTH_STATION_N) {
							waitForClearance(Semaphores.CROSSING);
						} else {
							setClearance(Semaphores.CROSSING);
						}
						break;
					case CROSSING_N:
						if (lastSensor == Sensors.NORTH_STATION_S) {
							waitForClearance(Semaphores.CROSSING);
						} else {
							setClearance(Semaphores.CROSSING);
						}
						break;
					case CROSSING_E:
						if (lastSensor == Sensors.EAST_W) {
							waitForClearance(Semaphores.CROSSING);
						} else {
							setClearance(Semaphores.CROSSING);
						}
						break;
					case CROSSING_S: 
						if (lastSensor == Sensors.EAST_S) {
							waitForClearance(Semaphores.CROSSING);
						} else {
							setClearance(Semaphores.CROSSING);
						}
						break;

					// East-top junction
					case EAST_W:
						if (lastSensor == Sensors.CROSSING_E) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(Semaphores.EAST_TRACK); // but with switch ??
							} else {
								// changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT); // may not be needed
								claim(Semaphores.EAST_TRACK);
								// maybe update speed
							}
						} else {
							setClearance(Semaphores.EAST_TRACK);
						}
						break;
					case EAST_S:
						if (lastSensor == Sensors.CROSSING_S) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(Semaphores.EAST_TRACK, Switches.EAST, tsi.SWITCH_LEFT); // but with switch
							} else {
								changeSwitch(Switches.EAST, tsi.SWITCH_LEFT);
								claim(Semaphores.EAST_TRACK);
								// maybe update speed
							}
						} else {
							setClearance(Semaphores.EAST_TRACK);
							System.out.println("east clearance set!!!!!!!!!!!!!!!!!");
							changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT); // kanske inte behvs
						}
						break;
					case EAST_E:
						if (lastSensor == Sensors.MID_E) {
							if (!semaphoreIsAvailable(Semaphores.N_STATION)) {
								changeSwitch(Switches.EAST, tsi.SWITCH_LEFT);
								// maybe update speed
							}
							else {
								claim(Semaphores.N_STATION);
								// maybe update switch
							}
						} else if (lastSensor == Sensors.EAST_W){
							setClearance(Semaphores.N_STATION); 
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
								claim(Semaphores.MID_TRACK);
							}	
						} else if (lastSensor == Sensors.MID_W) {
							setClearance(Semaphores.MID_TRACK);
						}  else {
							changeSwitch(Switches.MID, tsi.SWITCH_RIGHT);
						}
						break;
					case MID_W:
						System.out.println("At mid west sensor");
						if (lastSensor == Sensors.WEST_E) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)){
								waitForClearance(Semaphores.EAST_TRACK); // but with switch ??
							} else {
								claim(Semaphores.EAST_TRACK);
							}
						} else {
							setClearance(Semaphores.EAST_TRACK);
						}
						break;
					case MID_S:
						if (lastSensor == Sensors.WEST_S) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(Semaphores.EAST_TRACK, Switches.MID, tsi.SWITCH_LEFT); // but with switch
							} else {
								changeSwitch(Switches.MID, tsi.SWITCH_LEFT);
								claim(Semaphores.EAST_TRACK);
							}
						} else {
							changeSwitch(Switches.MID, tsi.SWITCH_RIGHT);
							setClearance(Semaphores.EAST_TRACK);
						}
						break;
						
					// West junction
					case WEST_W:
						if (lastSensor == Sensors.SOUTH_W) {
							if (!semaphoreIsAvailable(Semaphores.MID_TRACK)) {
								changeSwitch(Switches.WEST, tsi.SWITCH_RIGHT);
							} else {
								claim(Semaphores.MID_TRACK);
							}
						} else if (lastSensor == Sensors.WEST_E) {
							setClearance(Semaphores.MID_TRACK); 
						} else {
							
							changeSwitch(Switches.WEST, tsi.SWITCH_LEFT);
						}
						break;
					case WEST_E:
						if (lastSensor == Sensors.MID_W) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(Semaphores.WEST_TRACK); 
							} else {
								claim(Semaphores.WEST_TRACK);
							}
						} else {
							setClearance(Semaphores.WEST_TRACK); 
						}
						break;
					case WEST_S:
						if (lastSensor == Sensors.MID_S) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(Semaphores.WEST_TRACK, Switches.WEST, tsi.SWITCH_RIGHT); // but with switch
							} else {
								waitForClearance(Semaphores.WEST_TRACK, Switches.WEST, tsi.SWITCH_RIGHT);
//								changeSwitch(Switches.WEST, tsi.SWITCH_RIGHT);
//								claim(Semaphores.WEST_TRACK);
							}
						} else {
							changeSwitch(Switches.WEST, tsi.SWITCH_LEFT);
							setClearance(Semaphores.WEST_TRACK); 
						}
						break;
						
					// South junction
					case SOUTH_W:
						if (lastSensor == Sensors.WEST_W) {
							if (!semaphoreIsAvailable(Semaphores.S_STATION)) {
								changeSwitch(Switches.SOUTH, tsi.SWITCH_RIGHT);
							} else {
								claim(Semaphores.S_STATION);
							}
						} else if (lastSensor == Sensors.SOUTH_E){
							setClearance(Semaphores.S_STATION);
						} else {
						    changeSwitch(Switches.SOUTH, tsi.SWITCH_LEFT);
						}
						break;
					case SOUTH_E:
						if (lastSensor == Sensors.SOUTH_STATION_N) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(Semaphores.WEST_TRACK);
							} else {
								claim(Semaphores.WEST_TRACK);
							}
						} else {
							setClearance(Semaphores.WEST_TRACK);
						}
						break;
					case SOUTH_S:
						if (lastSensor == Sensors.SOUTH_STATION_S) {
							if (!semaphoreIsAvailable(Semaphores.WEST_TRACK)) {
								waitForClearance(Semaphores.WEST_TRACK, Switches.SOUTH, tsi.SWITCH_RIGHT); // but with switch
							} else {
								changeSwitch(Switches.SOUTH, tsi.SWITCH_RIGHT);
								claim(Semaphores.WEST_TRACK);
							}
						} else {
							changeSwitch(Switches.SOUTH, tsi.SWITCH_LEFT);
							setClearance(Semaphores.WEST_TRACK);
							System.out.println("clearance set on west track ----------------------------------");
							System.out.println(semaphores.get(Semaphores.WEST_TRACK).availablePermits());
						}
						break;
					
					// Stations
					case NORTH_STATION_N:
						if (lastSensor == Sensors.CROSSING_W) {
							System.out.println("WAIT north station n");
							waitAtStation();
						}
						break;
					case SOUTH_STATION_N:
						if (lastSensor == Sensors.SOUTH_E) {
							waitAtStation();
						}
						break;
					case NORTH_STATION_S:
						if (lastSensor == Sensors.CROSSING_N) {
							waitAtStation();
						}
						break;
					case SOUTH_STATION_S:
						if (lastSensor == Sensors.SOUTH_S) {
							waitAtStation();
						}
						break;
				}
				
				// System.out.println("new last sensor " + name);
				
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
				e.printStackTrace();
				System.exit(1);
			}
			catch (InterruptedException e) {
				e.printStackTrace();
				System.exit(1);
			}
			
			// TODO Auto-generated method stub
			
		}
		
		
		
	}
  
  
  
  
}


