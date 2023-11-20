import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import Building from 'src/app/core/models/building.model';
import Floor from 'src/app/core/models/floor.model';
import BuildingService from 'src/app/services/building.service';
import { FloorService } from 'src/app/services/floor.service';

@Component({
  selector: 'app-building-list',
  templateUrl: './building-list.component.html',
  styleUrls: ['./building-list.component.css'],
})
export class BuildingListComponent implements OnInit {
  buildings: Building[] = [];

  constructor(
    private buildingService: BuildingService,
    private floorService: FloorService,
    private router: Router,
  ) {}

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  createFloor(buildingCode: string) {
    console.log('create floor for bCode:', buildingCode);
    // this.buildingService.createFloor(buildingCode).subscribe((building) => {
    //   console.log(building);
    // });
    // Call building service or redirect to page
  }

  editBuilding(building: Building) {
    this.router.navigate(['campus/building', building.code, 'edit'], { state: { data: building } });
  }

  getByBuildingCode(buildingCode: string) {
    this.router.navigate(['campus/building', buildingCode, 'floors']);
  }

  listElevatorsInBuilding(buildingCode: string) {
    this.router.navigate(['campus/building', buildingCode, 'elevators']);
  }

  listFloorsWithPassagesToOtherBuildings(buildingCode: string) {
    this.router.navigate(['campus/building', buildingCode, 'floorsWithPassages']);
  }
}
