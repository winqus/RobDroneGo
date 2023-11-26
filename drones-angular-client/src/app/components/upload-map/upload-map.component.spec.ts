import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UploadMapComponent } from './upload-map.component';

describe('UploadMapComponent', () => {
  let component: UploadMapComponent;
  let fixture: ComponentFixture<UploadMapComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [UploadMapComponent]
    });
    fixture = TestBed.createComponent(UploadMapComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
