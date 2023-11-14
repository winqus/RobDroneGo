import { Component, OnInit } from '@angular/core';
import Building from 'src/app/core/models/building.model';
import { BuildingService } from 'src/app/services/building.service';

@Component({
  selector: 'app-building-list',
  templateUrl: './building-list.component.html',
  styleUrls: ['./building-list.component.css']
})
export class BuildingListComponent implements OnInit {
  buildings: Building[] = [];

  constructor(private buildingService: BuildingService) { }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings) => {
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

  editBuilding(buildingCode: string) {
    console.log('edit building for bCode:', buildingCode);
    // Call building service or redirect to page
  }
}
