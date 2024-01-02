import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import Building from '../../core/models/building.model';
import BuildingService from '../../services/building.service';

@Component({
  selector: 'app-building-list',
  templateUrl: './building-list.component.html',
  styleUrls: ['./building-list.component.css'],
})
export class BuildingListComponent implements OnInit {
  buildings: Building[] = [];

  constructor(
    private buildingService: BuildingService,
    private router: Router,
  ) {}

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  createFloor(buildingCode: string) {
    this.router.navigate(['campus/floor/create']);
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
