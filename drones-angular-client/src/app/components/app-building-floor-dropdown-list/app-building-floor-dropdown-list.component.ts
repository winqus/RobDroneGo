import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { Campus3dComponent } from '../campus3d/campus3d.component';

@Component({
  selector: 'app-building-floor-dropdown-list',
  templateUrl: './app-building-floor-dropdown-list.component.html',
  styleUrls: ['./app-building-floor-dropdown-list.component.css'],
})
export class AppBuildingFloorDropdownListComponent implements OnInit {
  @Output() onSelection: EventEmitter<any> = new EventEmitter<any>();
  buildings: Building[] = [];
  floorsByBuilding: { [buildingCode: string]: Floor[] } = {};

  constructor(
    private buildingService: BuildingService,
    private floorService: FloorService,
  ) {}

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings.sort((a, b) => a.code.localeCompare(b.code));
      buildings.forEach((building) => {
        this.floorService.getFloorsByBuildingCode(building.code).subscribe((floors: Floor[]) => {
          this.floorsByBuilding[building.code] = floors.sort((a, b) => a.floorNumber - b.floorNumber);
        });
      });
    });
  }

  onBuildingSelection(building: Building, floorNumber: number): void {
    this.onSelection.emit({ building: building, floorNumber: floorNumber });
  }

  getFloorsByBuilding(buildingCode: string): Floor[] {
    return this.floorsByBuilding[buildingCode] || [];
  }
  onFloorSelection(building: Building, floorNumber: number): void {
    this.onSelection.emit({ building: building, floorNumber: floorNumber });
  }
}
