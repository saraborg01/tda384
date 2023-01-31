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
	
	Map<Switches, List<Integer>> switches = new HashMap<>();
	Map<Semaphores, Semaphore> semaphores = new HashMap<>();
	Map<List<Integer>, Sensors> sensors = new HashMap<>();
	

    public Lab1(int speed1, int speed2) {
	    TSimInterface tsi = TSimInterface.getInstance();
	    
	    sensors.put(asList(6,6), Sensors.CROSSING_W);
	    sensors.put(asList(8,5), Sensors.CROSSING_N);
	    sensors.put(asList(10,7), Sensors.CROSSING_E);
	    sensors.put(asList(10,8), Sensors.CROSSING_S);
	    
	    sensors.put(asList(19,8), Sensors.EAST_E);
	    sensors.put(asList(15,7), Sensors.EAST_W);
	    sensors.put(asList(15,8), Sensors.EAST_S);
	    
	    sensors.put(asList(17,9), Sensors.MID_E);
	    sensors.put(asList(12,9), Sensors.MID_W);
	    sensors.put(asList(13,10), Sensors.MID_S);
	    
	    sensors.put(asList(7,9), Sensors.WEST_E);
	    sensors.put(asList(1,9), Sensors.WEST_W);
	    sensors.put(asList(6,10), Sensors.WEST_S);
	    
	    sensors.put(asList(5,11), Sensors.SOUTH_E);
	    sensors.put(asList(1,10), Sensors.SOUTH_W);
	    sensors.put(asList(4,13), Sensors.SOUTH_S);
	    
	    sensors.put(asList(15,3), Sensors.NORTH_STATION_N);
	    sensors.put(asList(15,5), Sensors.NORTH_STATION_S);
	    sensors.put(asList(15,11), Sensors.SOUTH_STATION_N);
	    sensors.put(asList(15,13), Sensors.SOUTH_STATION_S);
	    
	    switches.put(Switches.EAST,  asList(17,7));
	    switches.put(Switches.MID,   asList(15,9));
	    switches.put(Switches.WEST,  asList(4,9));
	    switches.put(Switches.SOUTH, asList(3,11));
	    
	    semaphores.put(Semaphores.CROSSING, new Semaphore(1));
	    semaphores.put(Semaphores.EAST_TRACK, new Semaphore(1));
	    semaphores.put(Semaphores.WEST_TRACK, new Semaphore(1));
	    semaphores.put(Semaphores.MID_TRACK, new Semaphore(1));
	    semaphores.put(Semaphores.N_STATION, new Semaphore(0));
	    semaphores.put(Semaphores.S_STATION, new Semaphore(0));
	    
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
		
		private static int MAX_SPEED = 20;
		
		public Train(int id, int speed, TSimInterface tsi, Sensors lastSensor) {
			this.id = id;
			this.tsi = tsi;
			this.lastSensor = lastSensor;
			this.speed = getMaxSpeed(Math.abs(speed));
		}
		
		/**
		 * Sets the maximum speed that the program allows.
		 * @param speed - the speed the user suggests
		 * @return
		 */
		private int getMaxSpeed(int speed) {
			
			if (speed > MAX_SPEED) {
				return MAX_SPEED;
			} else {
				return speed;
			}
		}
		
		/**
		 * Method to release an occupied semaphore.
		 * @param semName - the name of the semaphore.
		 */
	    private void setClearance(Semaphores semName) {
	    	Semaphore sem = semaphores.get(semName);
	    	if (sem.availablePermits() == 0 ) {
	    		sem.release();
			}
	    }	
	    
	    /**
	     * method to occcupy or acquire the given semaphore.
	     * @param semName - the name of the semaphore
	     * @throws InterruptedException
	     */
	    private void claim(Semaphores semName) throws InterruptedException{
	    	Semaphore sem = semaphores.get(semName);
	    	sem.acquire();
	    }

	    /**
	     * Method for the train to stop and wait until the given semaphore(track) is
	     * available and then go forward.
	     * @param semName
	     * @throws CommandException
	     * @throws InterruptedException
	     */
		private void waitForClearance(Semaphores semName) throws CommandException, InterruptedException{
			tsi.setSpeed(id, 0);
			claim(semName);
			tsi.setSpeed(id, speed);
		}
		
		/**
		 * Method for the train to stop and wait until the given semaphore(track) is
		 * available, then modify the switch, and then go forward.
		 * @param semName - name of the semaphore.
		 * @param s       - the switch that needs to be modified in order for the train to go onto the track.
		 * @param sDir    - the switch direction needed.
		 * @throws CommandException
		 * @throws InterruptedException
		 */
		private void waitForClearance(Semaphores semName, Switches s, int sDir) throws CommandException, InterruptedException{
			tsi.setSpeed(id, 0);
			claim(semName);
			changeSwitch(s, sDir);
			
			tsi.setSpeed(id, speed);
		}
		
		/**
		 * Switches the direction of the train.
		 * Used for when turning at the stations.
		 */
		private void switchDirection() {
			this.speed *= -1;
		}
		
		/**
		 * Method for the train to stop, and wait at a station. 
		 */
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
		
		/**
		 * Checks if a given semaphore(track) is available.
		 * @param semName - name of the semaphore to check.
		 * @return
		 */
		private boolean semaphoreIsAvailable(Semaphores semName) {
			Semaphore sem = semaphores.get(semName);
			// boolean notBusy = sem.tryAcquire();
			return sem.availablePermits() > 0;
		}
		
		/**
		 * Method for changing the direction of a switch.
		 * @param s         - the name of the switch to modify.
		 * @param direction - the new direction of the switch.
		 * @throws CommandException
		 */
		private void changeSwitch(Switches s, int direction) throws CommandException{
			List<Integer> pos = switches.get(s);
			tsi.setSwitch(pos.get(0), pos.get(1), direction);
		}
		
		/**
		 * Changes east and mid switch to direct on the defaukt path,
		 * which is the track that goes straight ahead.
		 * @throws CommandException
		 */
		private void setDefaultSwitches() throws CommandException{
	    	List<Integer> eastPos = switches.get(Switches.EAST);
	    	List<Integer> midPos = switches.get(Switches.MID);
	    	tsi.setSwitch(eastPos.get(0), eastPos.get(1), tsi.SWITCH_RIGHT);
	    	tsi.setSwitch(midPos.get(0), midPos.get(1), tsi.SWITCH_RIGHT);
	    }
		
		/**
		 * Handles all sensorevents that occur.
		 * @param event - the event that occurs when a sensor is activated or inactivated.
		 * @throws CommandException
		 * @throws InterruptedException
		 */
		private void handleSensor(SensorEvent event) throws CommandException, InterruptedException {
			Sensors name = sensors.get(asList(event.getXpos(), event.getYpos()));
			
			if (event.getStatus() == SensorEvent.ACTIVE) {
				switch (name) {
					
					/**
					 * Crossing
					 */
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

					/**
					 * East-top junction
					 */
					case EAST_W:
						if (lastSensor == Sensors.CROSSING_E) {
							if (!semaphoreIsAvailable(Semaphores.EAST_TRACK)) {
								waitForClearance(Semaphores.EAST_TRACK); 
							} else {
								claim(Semaphores.EAST_TRACK);
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
							}
						} else {
							setClearance(Semaphores.EAST_TRACK);
							changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT); 
						}
						break;
					case EAST_E:
						if (lastSensor == Sensors.MID_E) {
							if (!semaphoreIsAvailable(Semaphores.N_STATION)) {
								changeSwitch(Switches.EAST, tsi.SWITCH_LEFT);
							}
							else {
								claim(Semaphores.N_STATION);
							}
						} else if (lastSensor == Sensors.EAST_W){
							setClearance(Semaphores.N_STATION); 
						} else {
							changeSwitch(Switches.EAST, tsi.SWITCH_RIGHT);
						}
						break;
						
						
					/**
					 * Mid-east junction
					 */
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
								waitForClearance(Semaphores.EAST_TRACK);
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
								waitForClearance(Semaphores.EAST_TRACK, Switches.MID, tsi.SWITCH_LEFT); 
							} else {
								changeSwitch(Switches.MID, tsi.SWITCH_LEFT);
								claim(Semaphores.EAST_TRACK);
							}
						} else {
							changeSwitch(Switches.MID, tsi.SWITCH_RIGHT);
							setClearance(Semaphores.EAST_TRACK);
						}
						break;
						
					/**
					 * West junction
					 */
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
								waitForClearance(Semaphores.WEST_TRACK, Switches.WEST, tsi.SWITCH_RIGHT); 
							} else {
								changeSwitch(Switches.WEST, tsi.SWITCH_RIGHT);
								claim(Semaphores.WEST_TRACK);
							}
						} else {
							changeSwitch(Switches.WEST, tsi.SWITCH_LEFT);
							setClearance(Semaphores.WEST_TRACK); 
						}
						break;
						
					/**
					 * South junction
					 */
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
								waitForClearance(Semaphores.WEST_TRACK, Switches.SOUTH, tsi.SWITCH_RIGHT);
							} else {
								changeSwitch(Switches.SOUTH, tsi.SWITCH_RIGHT);
								claim(Semaphores.WEST_TRACK);
							}
						} else {
							changeSwitch(Switches.SOUTH, tsi.SWITCH_LEFT);
							setClearance(Semaphores.WEST_TRACK);
						}
						break;
					
					/**
					 * Stations
					 * When a train is coming into the stations, it has to wait.
					 */
					case NORTH_STATION_N:
						if (lastSensor == Sensors.CROSSING_W) {
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
			
		}
		
		
		
	}
  
  
  
  
}


