import { Component, OnInit } from '@angular/core';
import { UserRole } from 'src/app/core/authentication/models/user-roles.enum';
import { UserService } from 'src/app/core/authentication/services/user.service';
import { FileInfo, FolderService } from 'src/app/services/folder.service';

@Component({
  selector: 'app-public-folder',
  templateUrl: './public-folder.component.html',
  styleUrls: ['./public-folder.component.css'],
})
export class PublicFolderComponent implements OnInit {
  files: FileInfo[] = [];
  selectedFile: File | undefined;
  successMessage: string | null = null;
  isManager = false;

  constructor(
    private folderService: FolderService,
    private userService: UserService,
  ) {}

  ngOnInit(): void {
    this.folderService.listFiles().subscribe((files) => {
      this.files = files;
    });
    this.isManager = this.userService.hasRole([UserRole.CampusManager, UserRole.FleetManager, UserRole.SystemAdministrator, UserRole.TaskManager]);
  }

  downloadFile(target: EventTarget | null) {
    const element = target as HTMLButtonElement;
    console.log(element.value);
    return this.folderService.downloadFile(this.files.filter((file) => file.name === element.value)[0]).subscribe((data) => {
      const blob = new Blob([data], { type: 'application/octet-stream' });
      const url = window.URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = element.value;
      link.click();
    });
  }

  onFileSelected(event: any) {
    this.selectedFile = event.target.files[0];
  }

  uploadFile() {
    if (!this.selectedFile) {
      console.error('No file selected');
      return;
    }

    const formData = new FormData();
    formData.append('file', this.selectedFile);
    this.folderService.uploadFile(formData).subscribe((data) => {
      console.log(data);
      this.successMessage = 'File uploaded successfully';
    });

    this.ngOnInit();
  }
}
